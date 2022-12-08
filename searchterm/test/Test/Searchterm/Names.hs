{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Names (testNames) where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Searchterm.Interface.Core (ConTy (..), Forall (..), Inst, Strained (..), Ty (..), TyScheme (..), TyVar)
import Searchterm.Interface.Names (InferErr (..), inferKinds, namelessStrained)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testNames :: TestTree
testNames = testGroup "Names" [testInferKinds]

mkS :: [TyVar] -> [Inst TyVar] -> Ty TyVar -> TyScheme TyVar
mkS tvs insts body = TyScheme (Forall (Seq.fromList tvs) (Strained (Seq.fromList insts) body))

mkC :: ConTy TyVar -> [Ty TyVar] -> Ty TyVar
mkC hd args = TyCon hd (Seq.fromList args)

inf :: TyScheme TyVar -> Either InferErr (Seq (TyVar, Int))
inf sc = do
  case namelessStrained (unTyScheme sc) of
    Left err -> error ("Unexpected nameless err: " ++ show err)
    Right nf -> inferKinds (TyScheme nf)

testInferKinds :: TestTree
testInferKinds = testCase "inferKinds" $ do
  inf (mkS [] [] (mkC (ConTyKnown "Int") [])) @?= Right Empty
  inf (mkS ["a"] [] (mkC (ConTyKnown "Int") [])) @?= Right (Seq.singleton ("a", 0))
  inf (mkS ["a"] [] (mkC (ConTyFree "a") [])) @?= Right (Seq.singleton ("a", 0))
  inf (mkS ["a"] [] (mkC (ConTyKnown "Foo") [TyFree "a"])) @?= Right (Seq.singleton ("a", 0))
  inf (mkS ["m", "a"] [] (mkC (ConTyFree "m") [TyFree "a"])) @?= Right (Seq.fromList [("m", 1), ("a", 0)])
  inf (mkS ["m"] [] (mkC (ConTyFree "m") [TyFree "m"])) @?= Left (InferErrMismatch "m" 1 0)
