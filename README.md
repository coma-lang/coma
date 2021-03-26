# Coma

Coma is a language for manipulating data in the CSV files.

## Usage

### Test

```bash
stack test
```

Result should look something like this (taken from the latest test run):

```
coma> test (suite: coma-test)

CSV
  Parse
    normal CSV:             OK
    CSV with missing items: OK

All 2 tests passed (0.00s)

coma> Test suite coma-test passed
```

### Build

```bash
stack build
```

**Note:** After building, path to the executable will be printed out in the
terminal. It is quite long, so just copy-paste it or run a `cp` command to copy
the file out of there.

## Code Structure

```
coma/
--> app/
--> src/
--> test/
--> package.yaml
```

These are the most important locations you must know.

- [`app/`](app/) contains the `Main.hs` file and (potentially in the future)
  some more
  files related to the main executable logic.
- [`src/`](src/) contains all of the libraries and modules we create to empower
  Coma by
  abstracting away the details -- as all programmers do...
- [`test/`](test/) contains all of the test code and the main `Spec.hs` file
  that serves
  as the main registry of all functions we will be testing.
- [`package.yaml`](package.yaml) is the Stack manifest file where we specify all
  the dependencies we need (instead of a Cabal file -- do not edit it!).
