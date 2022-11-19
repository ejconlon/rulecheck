-- | Declarations of terms in our universe
module Searchterm.Interface.Decl where
  -- ( Partial (..)
  -- , Decl (..)
  -- , DeclErr
  -- , DeclSet (..)
  -- , mkDecl
  -- , mkDecls
  -- ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (StateT (..), gets, modify')
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Searchterm.Interface.Core (ClsName, Index (..), Inst (..), InstScheme (..), TmName (..), TyScheme (..),
                                 TyVar (..), instSchemeBody, tySchemeBody, Partial, tyToPartials, TyName, Ty, explodeTy)
import Searchterm.Interface.Names (NamelessErr, namelessInst, namelessType)
import Searchterm.Interface.Types (FuncLine (..), InstLine (..), Line (..), ConsLine (..))

-- | A declared term (essentially name and type scheme)
data Decl = Decl
  { declName :: !TmName
  -- ^ The name of the declared term (will be used in 'TmKnown' constructors)
  , declType :: !(TyScheme Index)
  -- ^ The type scheme of the term
  , declPartials :: !(Seq (Partial Index))
  -- ^ If this is a function declaration, types of partial applications
  } deriving stock (Eq, Ord, Show)

-- | A contructor signature - name of the constructor and type information
data ConSig = ConSig
  { csName :: !TmName
  -- ^ Constructor name
  , csScheme :: !(TyScheme Index)
  -- ^ Type scheme of the constructed type (does not include arguments to constructor)
  , csVars :: !(Seq (Ty Index))
  -- ^ Vars with type indices possibly bound in the scheme
  } deriving stock (Eq, Ord, Show)

data DeclSet = DeclSet
  { dsMap :: !(Map TmName Decl)
  -- ^ Map from term to its definition
  , dsDeps :: !(Map ClsName (Seq (InstScheme Index)))
  -- ^ Map from class name to instance schemes
  , dsCons :: !(Map TyName (Seq ConSig))
  -- ^ Map from type name to constructors
  } deriving stock (Eq, Show)

emptyDeclSet :: DeclSet
emptyDeclSet = DeclSet Map.empty Map.empty Map.empty

data DeclErr =
    DeclErrNameless !(Maybe TmName) !(NamelessErr TyVar)
  | DeclErrDupeTm !TmName
  | DeclErrDupeCons !TyName
  | DeclErrInst !(Inst TyVar) DeclErr
  | DeclErrMissingCon !TmName
  | DeclErrForbidStraint !TmName
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

mkDecl :: TmName -> TyScheme TyVar -> Either DeclErr Decl
mkDecl n s = do
  case namelessType s of
    Left e -> Left (DeclErrNameless (Just n) e)
    Right s' -> do
      let ps = tyToPartials (tySchemeBody s')
      pure (Decl n s' ps)

type DeclM a = StateT DeclSet (Except DeclErr) a

runDeclM :: DeclM a -> DeclSet -> Either DeclErr (a, DeclSet)
runDeclM m = runExcept . runStateT m

execDeclM :: DeclM () -> DeclSet -> Either DeclErr DeclSet
execDeclM m = fmap snd . runDeclM m

insertTmDeclM :: TmName -> TyScheme TyVar -> DeclM ()
insertTmDeclM n s =
  case mkDecl n s of
    Left e -> throwError e
    Right d -> do
      m <- gets dsMap
      case Map.lookup n m of
        Just _ -> throwError (DeclErrDupeTm n)
        Nothing -> modify' (\ds -> ds { dsMap = Map.insert n d (dsMap ds) })

insertInstM :: InstScheme TyVar -> DeclM ()
insertInstM is = do
  case namelessInst is of
    Left e -> throwError (DeclErrInst (instSchemeBody is) (DeclErrNameless Nothing e))
    Right is' -> do
      let cn = instName (instSchemeBody is')
      modify' (\ds -> ds { dsDeps = Map.alter (Just . maybe (Seq.singleton is') (:|> is')) cn (dsDeps ds) })

declToConSig :: Decl -> DeclM ConSig
declToConSig (Decl nm ty _) =
  case explodeTy ty of
    Nothing -> throwError (DeclErrForbidStraint nm)
    Just (tyEnd, argTys) -> pure (ConSig nm tyEnd argTys)

prepSigM :: TmName -> DeclM ConSig
prepSigM cn = do
  tms <- gets dsMap
  case Map.lookup cn tms of
    Nothing -> throwError (DeclErrMissingCon cn)
    Just decl -> declToConSig decl

insertConsM :: TyName -> Seq TmName -> DeclM ()
insertConsM tn cns = do
  cons <- gets dsCons
  case Map.lookup tn cons of
    Just _ -> throwError (DeclErrDupeCons tn)
    Nothing -> do
      sigs <- traverse prepSigM cns
      modify' (\ds -> ds { dsCons = Map.insert tn sigs cons })

insertLineM :: Line -> DeclM ()
insertLineM = \case
    LineFunc (FuncLine n ts) -> insertTmDeclM n ts
    LineInst (InstLine is) -> insertInstM is
    LineCons (ConsLine tm cns) -> insertConsM tm cns
    -- Do we need to add anything else to the decl set for search?
    _ -> pure ()

mkDecls :: [(TmName, TyScheme TyVar)] -> Either DeclErr DeclSet
mkDecls = flip execDeclM emptyDeclSet . traverse_ (uncurry insertTmDeclM)

mkLineDecls :: [Line] -> Either DeclErr DeclSet
mkLineDecls = flip execDeclM emptyDeclSet . traverse_ insertLineM
