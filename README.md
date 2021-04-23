# Coma

Coma is a language for manipulating data in the CSV files.

## Code Structure

```
coma/
--> app/
--> src/
--> test/
--> package.yaml
--> problems/
--> syntax/
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
- [`problems/`](problems/) contains solutions to problems with the Coma
  programming language.
- [`syntax/`](syntax/) contains BNF syntax declaration, Alex and Happy files.

## Usage

### Test

```bash
stack test
```

Result should look something like this (taken from one of the first test runs):

```
coma> test (suite: coma-test)

CSV
  Parse
    normal CSV:             OK
    CSV with missing items: OK

All 2 tests passed (0.00s)

coma> Test suite coma-test passed
```

### Execute

You can execute the program, without building it first, as follows:

```bash
stack exec csvql [ARGUMENTS]
```

For example, you can try this:

```bash
stack exec csvql problems/p1.cql
```

Now, you can also start a REPL with

```bash
stack exec csvql repl
```

Exit the REPL session with either `CTRL+C` or `CTRL+D` (although the second
might not work on Windows).

### Install

#### Windows

```bash
alex  syntax/Lexer.x  -o src/Lexer.hs
happy syntax/Parser.y -o src/Ast.hs
stack install
```

#### Linux

```bash
bash install.sh
```

#### MacOS

```bash
zsh install.sh
```
