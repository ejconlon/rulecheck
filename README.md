# rulecheck

## Quick Start

### Obtaining Packages

To start, we need to download some Haskell packages to fuzz. This is most easily
accomplished by the tools in the `auto` directory. You can use `auto` to
download packages containing rewrite ruless. Here's how to do it in three steps
(from the auto directory):

```sh
./run.sh find --listing $LISTINGFILE
./run.sh download --listing $LISTINGFILE --scratch $PACKAGEDIR
./run.sh extract --listing $LISTINGFILE --scratch $PACKAGEDIR --output $RULESFILE
```

where `$LISTINGFILE` is a temporary file to store the package information from
hackage. `$PACKAGEDIR` is where the packages will be downloaded, and
`$RULESFILE` contains information on packages containing rewrite rules. 

### Generating a test suite for package(s)

The `rulecheck` project is responsible for generating a test suite for the
rewrite rules in a given package. Before using it, you should tell it the
appropriate values for `$PACKAGEDIR` and `$RULESFILE`. This can be done by
modifying appropriate values in the `Config.hs` file.

To generate a suite for a single package `$PACKAGENAME`, run the command:

``` sh
stack run -- $PACKAGENAME
```

If you want to generate a suite for every package in `$RULESFILE`, this can be
done by running without any arguments:

``` sh
stack run 
```

### Running a fuzzing test suite for a package

After generating a test suite for `$PACKAGENAME`, you can run the test suite by
taking two steps:

1. Add the package `package-tests/$PACKAGENAME-test` to `rulecheck/stack.yaml`
2. Run the command `stack test $PACKAGENAME-test`

## Debugging

Various issues can arise during both test suite generation and running the
generated test suites. Below are some common issues, and how they can be addressed.

### Test Generation Problems

The test generation logic runs separately for each file in a project that
contains rewrite rules. However, some files actually shouldn't be analyzed for
the following reasons:

- The file is actually a test file (for programs that analyze Haskell code, for example)
- The rewrite rule only applies to other version of GHC besides 9.0.2 
- The rule isn't actually a rule at all for example it's been commented out.
  This is possible because rule identification just uses a regex.
  
These files can be skipped by adding an entry in `filesToSkip` in the
`Config.hs`. Note that the `filesToSkip` entry matching is performed via an
infix match, so facilitating skipping multiple files at once.

If a whole package seems to have a problem, you can also skip it via the
`packagesToSkip` in `Config.hs`.

### Test Suite Problems

A common issue is that imports are not in scope; we haven't figured out a way to
automatically add all relevant imports (yet). The `importsForPackage` function
in `Config.hs` defines what imports should be added for each test file for a
package. You can modify that function to add necessary imports and then
re-generate the test suite.

Another possible issue is that the rules cannot be tested because their types
are not exposed. To fix this you will need to:

1. modify the package under test itself (i.e. the one in `$PACKAGEDIR`) to expose the necessary modules, and
2. instruct rulecheck to use the modified package.

(1) should only require modifying the .cabal file to expose the module. (2) can
be done by modifying `stack.yaml` to include the directory for that package, so
the one from Hackage is not used.

## Pre-submit checks

Basically hlint should be happy and the test suites should pass:

    make lint test

You can format (if you have `stylish-haskell` installed) with:

    make format

## Links

* [ghc-lib](https://www.stackage.org/lts-19.25/package/ghc-lib-9.0.2.20211226)
* [ghc-lib-parser](https://www.stackage.org/lts-19.25/package/ghc-lib-parser-9.0.2.20211226)
