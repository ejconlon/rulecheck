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
                                 TyVar (..), instSchemeBody, tySchemeBody, Partial, tyToPartials, TyName, Ty)
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
    DeclErrNameless !(NamelessErr TyVar)
  | DeclErrDupe
  | DeclErrNamed !TmName DeclErr
  | DeclErrInst !(Inst TyVar) DeclErr
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

mkDecl :: TmName -> TyScheme TyVar -> Either DeclErr Decl
mkDecl n s = do
  case namelessType s of
    Left e -> Left (DeclErrNameless e)
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
    Left e -> throwError (DeclErrNamed n e)
    Right d -> do
      m <- gets dsMap
      case Map.lookup n m of
        Just _ -> throwError (DeclErrNamed n DeclErrDupe)
        Nothing -> modify' (\ds -> ds { dsMap = Map.insert n d (dsMap ds) })

insertInstM :: InstScheme TyVar -> DeclM ()
insertInstM is = do
  case namelessInst is of
    Left e -> throwError (DeclErrInst (instSchemeBody is) (DeclErrNameless e))
    Right is' -> do
      let cn = instName (instSchemeBody is')
      modify' (\ds -> ds { dsDeps = Map.alter (Just . maybe (Seq.singleton is') (:|> is')) cn (dsDeps ds) })

insertConsM :: TyName -> Seq TmName -> DeclM ()
insertConsM _tn _cns = pure () -- TODO

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
