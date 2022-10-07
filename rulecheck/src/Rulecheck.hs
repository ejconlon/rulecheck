module Rulecheck where

import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule (ruleFromDecl, ruleModuleDoc)
import Rulecheck.Typecheck (getTypecheckedRuleDecls, typecheck)

generateFile :: FilePath -> String -> FilePath -> IO ()
generateFile srcFile genModName _genModFile = do
  modContents <- cradleGhcM srcFile $ do
    tcm <- typecheck srcFile
    let tcRules = getTypecheckedRuleDecls tcm
    rules <- traverse ruleFromDecl tcRules
    modDoc <- ruleModuleDoc genModName rules
    outputString modDoc
  putStrLn modContents
  -- TODO write the file
  -- writeFile genModFile modContents

main :: IO ()
main = do
  -- Placeholder - just generate our fixed target for now
  let srcFile = "demo-domain/src/DemoDomain.hs"
      genModName = "DemoTest.Generated.DemoDomain"
      genModFile = "demo-test/test/DemoTest/Generated/DemoDomain.hs"
  generateFile srcFile genModName genModFile
