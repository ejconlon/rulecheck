{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Control.Exception ( catch, throwIO )
import Control.Monad.State
import Data.Aeson ( eitherDecodeFileStrict )
import Data.Either ( fromRight )
import Data.Functor.Foldable ( cata )
import Data.List ( isInfixOf, nub )
import Data.List.Utils ( replace )
import Data.Set ( Set )
import qualified Data.Set as Set ( fromList, map, union )
import qualified Data.Text as T
    ( Text, concat, pack, append, isPrefixOf, lines, null, unpack )
import Data.Text ( Text )
import qualified Data.Text.IO as TIO ( readFile )
import Debug.Trace ( trace )
import Prettyprinter ( Pretty(pretty) )
import Rulecheck.Config
    ( arbitraryInstanceFile,
      filesToSkip,
      haskellPackagesDir,
      importsForPackage,
      internalToPublicMod,
      overrideTypeSigs,
      packageDescriptionsFile,
      packageDir,
      packagesToSkip,
      skipRule,
      startFromPackage,
      testBaseDir,
      testGenDir,
      testSrcDir,
      testTemplateFile,
      PackageDescription(version, files, name) )
import Rulecheck.Monad ( cradleGhcM, GhcM )
import Rulecheck.Rendering
    ( identifyRequiredImports, outputString )
import Rulecheck.Rule ( Rule(ruleName, ruleTermArgs) )
import Rulecheck.RuleRendering
    ( TestModuleRenderOpts(..), ruleInputDoc, ruleModuleDoc )
import Rulecheck.RuleExtraction ( getRulesFromFile )
import Searchterm.Synth.Search
    ( runSearchN,
      Found(foundTm, foundTy),
      SearchConfig(SearchConfig),
      TmUniq(TmUniq),
      TyFoundScheme(unTyFoundScheme),
      UseSkolem(UseSkolemNo) )
import Searchterm.Util ( rethrow, inlineLets )
import Searchterm.Interface.Core
    ( Tm(..),
      TmF(TmCaseF, TmFreeF, TmLitF, TmKnownF, TmAppF, TmLamF, TmLetF),
      TmName(TmName),
      PatPair(PatPair) )
import Searchterm.Interface.Decl ( mkLineDecls, DeclSet )
import Searchterm.Interface.ParenPretty ( docToText )
import Searchterm.Interface.Parser
    ( ParseErr, parseLine, parseType )
import Searchterm.Interface.Printer ( printTerm )
import Searchterm.Interface.Names
    ( namelessClosedTerm, namelessType, renameTerm )
import Searchterm.Interface.Types ( Line )
import System.Directory
    ( copyFile,
      createDirectoryIfMissing,
      doesDirectoryExist,
      doesFileExist,
      removeDirectoryRecursive )
import System.Environment ( getArgs )
import System.Environment.Blank ( getEnvDefault )
import Text.Printf ( printf )

packageFilePath :: String -> PackageDescription -> String -> FilePath
packageFilePath prefix pkg file =
  prefix ++  "/" ++ name pkg ++ "-" ++ version pkg ++ "/" ++ file

data GenerateOptions =
  GenerateOptions
    { srcFile    :: FilePath
    , genModName :: String
    , genDeps    :: Set String
    , genModFile :: FilePath
    , genDecls   :: Maybe DeclSet
    }

getModContents :: GenerateOptions -> IO String
getModContents (GenerateOptions {genDecls, srcFile, genModName, genDeps}) =
  cradleGhcM srcFile $ do
    rulesInFile <- getRulesFromFile srcFile
    let rules = filter (not . skipRule srcFile . ruleName) rulesInFile
    case genDecls of
      Just decls -> mapM_ (getSynthResultsForRule decls) (filter (not . null . ruleTermArgs) rules)
      Nothing -> return ()

    let renderOpts = TestModuleRenderOpts genModName genDeps (overrideTypeSigs srcFile)

    -- `modDoc` is a "preliminary" version of the output test suite that may not
    -- include all necessary module inputs
    let modDoc = ruleModuleDoc renderOpts rules

    -- Determines the necessary module imports by identifying all of the Name's
    -- that need to be rendered in `modDoc`
    additionalImports <- Set.map internalToPublicMod <$> identifyRequiredImports modDoc

    -- Incorporate additional imports for re-render
    let renderOpts' = renderOpts{testImports = Set.union genDeps additionalImports}

    -- Final version of the test suite, with additional imports
    let modDoc' = ruleModuleDoc renderOpts' rules
    contents <- outputString modDoc'
    return $ "-- Generated for rules in file " ++ srcFile ++ "\n" ++ contents

generateFile :: GenerateOptions -> IO ()
generateFile opts = do
    modContents <- getModContents opts
    writeFile (genModFile opts) modContents

demoGenOpts :: GenerateOptions
demoGenOpts = GenerateOptions
  "demo-domain/src/DemoDomain.hs"
  "DemoTest.Generated.DemoDomain"
  (Set.fromList ["DemoDomain"])
  "demo-test/test/DemoTest/Generated/DemoDomain.hs"
  Nothing

getGenerateOptions :: Maybe DeclSet -> FilePath -> Int -> PackageDescription -> GenerateOptions
getGenerateOptions ds path num desc =
  GenerateOptions
    path
    ("Rulecheck.Generated.Test" ++ show num)
    (Set.union (importsForPackage desc) defaultImports)
    (testGenDir desc ++ "/Test" ++ show num ++ ".hs")
    ds
  where
    defaultImports :: Set String
    defaultImports = Set.fromList ["GHC.Base", "GHC.Float", "GHC.Types", "GHC.Word"]


assertFileExists :: FilePath -> IO ()
assertFileExists path = do
      exists <- doesFileExist path
      unless exists $ error ("File " ++ path ++ " does not exist")

shouldSkip :: FilePath -> Bool
shouldSkip path = any (`isInfixOf` path) filesToSkip


copyTemplateFile :: PackageDescription -> FilePath -> IO ()
copyTemplateFile desc path = do
    pDir <- packageDir desc
    yamlTemplate <- readFile (testTemplateFile path)
    let contents =
          replace "{testname}" (name desc ++ "-test") $
          replace "{packagename}" (name desc) $
          replace "{packagedir}"pDir yamlTemplate
    writeFile (testBaseDir desc ++ "/" ++ path) contents

setupTestDirectory :: PackageDescription -> IO ()
setupTestDirectory pkg =
  do
    dirExists <- doesDirectoryExist (testBaseDir pkg)
    when dirExists $ removeDirectoryRecursive (testBaseDir pkg)
    createDirectoryIfMissing True (testGenDir pkg) -- This will create all intermediate dirs
    copyFile (testTemplateFile "Main.hs") (testSrcDir pkg ++ "/Main.hs")
    copyTemplateFile pkg "package.yaml"
    copyTemplateFile pkg "stack.yaml"
    instancesFilename <- arbitraryInstanceFile pkg
    copyFile instancesFilename (testGenDir pkg ++ "/ArbitraryInstances.hs")


declSetForPackage :: String -> PackageDescription -> IO (Maybe DeclSet)
declSetForPackage prefix pkg = do
  hasDeclsFile <- doesFileExist declsFile
  if hasDeclsFile
    then do
      printf "%s has decls file\n" (name pkg)
      fmap Just (loadDecls (Just declsFile))
    else printf "%s has does not have decls\n" (name pkg) >> return Nothing
  where
    declsFile = packageFilePath prefix pkg "defs.txt"

processPackage :: String -> PackageDescription -> IO ()
processPackage prefix pkg = do
  putStrLn $ "Processing " ++ name pkg
  ds <- declSetForPackage prefix pkg
  setupTestDirectory pkg
  mapM_ (go ds) (zip (map (packageFilePath prefix pkg) $ files pkg) [1..])
  where
    go :: Maybe DeclSet -> (FilePath, Int) -> IO ()
    go _ (path, _) | shouldSkip path = putStrLn $ "Skipping file at path" ++ path
    go ds (path, n) = do
      assertFileExists path
      putStrLn $ "Processing file " ++ path
      generateFile (getGenerateOptions ds path n pkg)

getPackagesToProcess :: [String] -> IO [PackageDescription]
getPackagesToProcess packages = do
  jsonResult <- eitherDecodeFileStrict packageDescriptionsFile
  return $ case jsonResult of
    Left err           -> error err
    Right descriptions -> go packages
      where
        go [] = filter (not . skip) $ case startFromPackage of
          Just p  -> dropWhile ((p /=) . name) descriptions
          Nothing -> descriptions
        go _  = filter ((`elem` packages) . name) descriptions

        skip package = name package `elem` packagesToSkip

rulecheck :: [String] -> IO ()
rulecheck args = do
  dir <- haskellPackagesDir
  toProcess <- getPackagesToProcess args
  mapM_ (processPackage dir) toProcess
  putStrLn "Done"

loadFileLines :: FilePath -> IO [Line]
loadFileLines fp = do
  contents <- TIO.readFile fp
  return $ concatMap go $ T.lines contents
  where
    go :: T.Text -> [Line]
    go t | T.null t = []
    go t | Right e <- parseLine fp t = [e]
    go t = trace ("Could not parse line " ++ T.unpack t) []

data DoGen a b = DoGen [a] b

returnText :: (Pretty a) => DoGen a (Tm a a) -> Text
returnText (DoGen _ ret) = T.append "return " (printTerm ret)

renderDoGen :: (Pretty a) => DoGen a (Tm a a) -> Text
renderDoGen d@(DoGen [] _)      = returnText d
renderDoGen d@(DoGen clauses _) = T.concat $ ("do\n" : map go clauses) ++ [T.append "  " $ returnText d]
  where
    go t = T.concat ["  " , (docToText . pretty) t , " <- arbitrary\n"]

mkDoGen :: Tm String String -> DoGen String (Tm String String)
mkDoGen t = uncurry (flip DoGen) $ runState (go t) [] where
  freshDoVar typName = do
    prevVars <- get
    let index = length prevVars + 1
    let varName = "gen_" ++ typName ++ show index
    put (prevVars ++ [varName])
    return varName
  go = cata goTm
  goTm = \case
    TmFreeF a -> pure (TmFree a)
    TmLitF l -> pure (TmLit l)
    TmKnownF (TmName n) | "rcgen__" `T.isPrefixOf` n ->
      do
        v <- freshDoVar $ drop 7 $ T.unpack n
        return $ TmKnown (TmName (T.pack v))
    TmKnownF n -> pure (TmKnown n)
    TmAppF wl wr -> TmApp <$> wl <*> wr
    TmLamF b w -> TmLam b <$> w
    TmLetF x arg body -> TmLet x <$> arg <*> body
    TmCaseF scrut pairs -> TmCase <$> scrut <*> traverse goPair pairs
  goPair (PatPair pat w) = PatPair pat <$> w

getSynthResultsForType :: String -> DeclSet -> IO [(Tm String String, T.Text)]
getSynthResultsForType typName ds = do
  tsNamed <- either throwTypNameParseErr pure (parseType (T.pack typName))
  ts <- rethrow (namelessType tsNamed)
  maxSearchDepth <- fmap read (getEnvDefault "RC_MAX_SEARCH_DEPTH" "10")
  numResults <- fmap read (getEnvDefault "RC_NUM_RESULTS" "5")
  let useSkolem = UseSkolemNo
  let config = SearchConfig ds ts maxSearchDepth useSkolem
  case runSearchN config numResults of
    Left err      -> error (show err)
    Right results -> do
      printf "%d terms found for type %s\n" (length results) typName
      return $ map convert results
      where
        convert result = (tm, ty)
          where
            mkVarName (TmUniq i) = "x" ++ show i
            namelessTm           = fromRight undefined $ namelessClosedTerm (const Nothing) $ inlineLets $ foundTm result
            tm                   = fromRight undefined $ renameTerm mkVarName namelessTm
            ty                   = docToText $ pretty $ unTyFoundScheme $ foundTy result
  where
    throwTypNameParseErr err = do
      printf "Error: cannot parse type %s for synthesis\n" typName
      throwIO err

getSynthResultsForRule :: DeclSet -> Rule -> GhcM [(Tm String String, T.Text)]
getSynthResultsForRule decls rule = do
  typName <- outputString (ruleInputDoc rule)
  liftIO $ printf "Synthesizing input terms for rule %s (type:  %s)\n" (show $ ruleName rule) (typName)
  liftIO $ catch (getSynthResultsForType typName decls) reportErr
  where
    reportErr :: ParseErr -> IO [(Tm String String, T.Text)]
    reportErr _ = do
      putStrLn "Ignoring parse error for rule"
      return []

searchWithDecls :: String -> DeclSet -> IO ()
searchWithDecls typName ds = do
  sr <- getSynthResultsForType typName ds
  mapM_ showResult sr
  where
    showResult (tm, ty) = printf "Term: %s\nType:%s\n" (renderDoGen $ mkDoGen tm) ty


loadDecls :: Maybe FilePath -> IO DeclSet
loadDecls pkgDefsPath = do
  baseLines <- loadFileLines "searchterm/prelude.txt"
  pkgLines  <- case pkgDefsPath of
    Just path -> loadFileLines path
    Nothing   -> return []
  rethrow (mkLineDecls $ nub $ baseLines ++ pkgLines)


searchterm :: [String] -> IO ()
searchterm [filename, typName] = do
  decls <- loadDecls $ Just filename
  searchWithDecls typName decls
searchterm [typName] = do
  decls <- loadDecls Nothing
  searchWithDecls typName decls
searchterm _ = putStrLn "Usage stack run -- --searchterm DEF_FILE TYP"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--searchterm" : stArgs) -> searchterm stArgs
    _ -> rulecheck args
