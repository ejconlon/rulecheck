module Rulecheck where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Typecheck (getTypecheckedRuleDecls, typecheck)

main :: IO ()
main = do
  -- This is just a placeholder for the real thing
  let demoDomainFile = "demo-domain/src/DemoDomain.hs"
  cradleGhcM demoDomainFile $ do
    tcm <- typecheck demoDomainFile
    let rules = getTypecheckedRuleDecls tcm
    reps <- traverse outputString rules
    for_ reps (liftIO . putStrLn)
