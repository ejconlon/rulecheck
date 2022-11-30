# rulecheck

## Quick Start

### Obtaining Packages

To start, we need to download some Haskell packages to fuzz. This is most easily
accomplished by the tools in the `auto` directory. You can use `auto` to
download packages containing rewrite ruless. Here's how to do it in three steps
(from the auto directory):

```sh
./run.sh main find --listing $LISTINGFILE
./run.sh main download --listing $LISTINGFILE --scratch $PACKAGEDIR
./run.sh main extract --listing $LISTINGFILE --scratch $PACKAGEDIR --output $RULESFILE
```

where `$LISTINGFILE` is a temporary file to store the package information from
hackage. `$PACKAGEDIR` is where the packages will be downloaded, and
`$RULESFILE` contains information on packages containing rewrite rules.

__IMPORTANT__ Current we expect `$PACKAGEDIR` to be named `haskell-packages` and
placed adjacent to this directory (i.e. `../haskell-packages`). __AND__ you need
to put the full path in `haskellPackagesDir` in `Config.hs`.

All this can also be done with `./prepare.sh`.

### Generating and Running the Test Suites

You can automatically generate and run test suites for modules we now how to
fuzz by running the script `./test-all-working.sh`

## Generating New Test Suites

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

After generating a test suite for `$PACKAGENAME`, the appropriate tests will be
created in `package-tests/$PACKAGENAME-test`.

You can then easily run the fuzz test by running `stack test` from inside the
generated directory.

### Generating and running

The above two steps can be done at once with the command `./test.sh $PACKAGENAME`

### Adding a suite to the set of working tests

Just edit `./test-all-working.sh`, it should be straightforward

### Adding custom `Arbitrary` instances

This can be done by adding a file in
`test-template/extra-arbitrary-instances/$PACKAGENAME.hs`.

### Modifying the library

You may want to do this to expose additional modules, functions, etc.
This can be done by putting modified versions of the file in the `vendored`
directory. The script `./apply-vendored-patches.sh` will apply the patches.

## Debugging

Various issues can arise during both test suite generation and running the
generated test suites. Below are some common issues, and how they can be addressed.

### Test Generation Problems

The test generation logic runs separately for each file in a project that
contains rewrite rules. However, some files actually shouldn't be analyzed for
the following reasons:

- The file is actually a test file (for programs that analyze Haskell code, for example)
- The rewrite rule only applies to other version of GHC besides 9.0.2
- The rule isn't actually a rule at all (for example, it could been commented out).
  This is possible because rule identification uses a regular expression and
  could have false positives.

These files can be skipped by adding an entry in `filesToSkip` in the
`Config.hs`. Note that the `filesToSkip` entry matching is performed via an
infix match, so facilitating skipping multiple files at once.

If a whole package seems to have a problem, you can also skip it via the
`packagesToSkip` in `Config.hs`.

### Test Suite Problems

It's possible that required imports are not in scope. The `importsForPackage`
function in `Config.hs` defines what imports should be added for each test file
for a package. You can modify that function to add necessary imports and then
re-generate the test suite.

## Pre-submit checks

Basically hlint should be happy and the test suites should pass:

    make lint test

You can format (if you have `stylish-haskell` installed) with:

    make format

## Links

* [ghc-lib](https://www.stackage.org/lts-19.25/package/ghc-lib-9.0.2.20211226)
* [ghc-lib-parser](https://www.stackage.org/lts-19.25/package/ghc-lib-parser-9.0.2.20211226)
