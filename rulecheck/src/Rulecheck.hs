module Rulecheck where

import Data.Set (Set)
import qualified Data.Set as Set
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule (ruleFromDecl, ruleModuleDoc)
import Rulecheck.Typecheck (getTypecheckedRuleDecls, typecheck)

generateFile :: FilePath -> String -> Set String -> FilePath -> IO ()
generateFile srcFile genModName genDeps genModFile = do
  modContents <- cradleGhcM srcFile $ do
    tcm <- typecheck srcFile
    let tcRules = getTypecheckedRuleDecls tcm
    rules <- traverse ruleFromDecl tcRules
    modDoc <- ruleModuleDoc genModName genDeps rules
    outputString modDoc
  -- putStrLn modContents
  writeFile genModFile modContents

main :: IO ()
main = do
  -- Placeholder - just generate our fixed target for now
  let srcFile = "demo-domain/src/DemoDomain.hs"
      genModName = "DemoTest.Generated.DemoDomain"
      genModFile = "demo-test/test/DemoTest/Generated/DemoDomain.hs"
      genDeps = Set.fromList ["DemoDomain"]
  generateFile srcFile genModName genDeps genModFile
