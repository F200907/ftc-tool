# Finite-trace Tool

**This implementation serves as a proof-of-concept and is not maintained.**

The companion repository of the theoretical framework outlined in my master thesis.

- [Finite-trace Tool](#finite-trace-tool)
  - [Building](#building)
  - [Usage](#usage)
    - [Syntax](#syntax)
    - [Known Issues](#known-issues)
  - [Reference](#reference)

## Building

The project has been tested with the following dependencies.
Make sure you have the programs installed:

- [GHC 9.4.8](https://www.haskell.org/ghc/)
- [Stack 3.3.1](https://docs.haskellstack.org/en/stable/)
- [Z3 4.14.0](https://github.com/Z3Prover/z3)

In order to install a binary link `trace-tool-exe` execute the following command.
The initial compilation may take some time as all dependencies need to be fetched and built.

```sh
stack install
```

Alternatively, the tool can be run from this directory by replacing `trace-tool-exe` by `stack run --`.
For example, instead of `trace-tool-exe --help` you can use `stack run -- --help`.

## Usage

The tool is most commonly called by `trace-tool-exe -p -f FILENAME`.

We highly recommend always using the pretty flag `-p` if your terminal supports rendering unicode.
The pretty formatted output is closer to the formal definitions, whereas the default printing follows the internal structure of the implementation.

| Option | Flag | Description |
| - | - | - |
| Input | `--file FILENAME` | The file location of the problem instance. |
| Pretty Printing | `--pretty` | Replaces Haskell's derived `Show` implementations by pretty printing with infix notation and unicode operators. |
| SMT-Debug | `--debug` | Prints the SMT-LIB commands in the standard output. |
| Reinforce Contracts | `--reinforce` | Introduces the constraint `#x = x` for every variable `x` occurring in the procedure's body, but not in the postcondition. |
| Help | `--help` | Prints the help text (see below). |

The tool's help can be accessed with the flag `--help`.

```sh
> trace-tool-exe --help

Usage: trace-tool-exe [-f|--file FILENAME] [-p|--pretty] [-d|--debug] 
                      [-r|--reinforce]

Available options:
  -f,--file FILENAME       Input file (if none then STDIN is used)
  -p,--pretty              Pretty printing
  -d,--debug               Print debugging
  -r,--reinforce           Reinforce the program's contracts
  -h,--help                Show this help text
```

### Syntax

```rec
"SPECIFICATION"

[PRECONDITION]
[POSTCONDITION]
m {
    S_m
}

...

S
```

```grammar
a ::= x | #x | n
  | -a | a + a | a - a | a * a
```

```grammar
b ::= true | false
  | a < a | a <= a | a > a | a >= a | a = a | a != a 
  | !b | b & b | (b | b)
```

```grammar
tf ::= b | Id | Sb_x^a | X_r 
   | tf && tf | tf || tf | tf ~ tf | \X_r.tf
```

```grammar
s ::= skip | x := a | s; s | if b then s else s | m()
```

### Known Issues

- Parsing an arithmetic expression may require an additional set of parenthesis, e.g., `Sb_x^0` raises an error whereas `Sb_x^(0)` is parsed correctly.

## Reference

```bibtex
@mastersthesis{Stevanovic2025,
    author = {Filip Stevanovic},
    title = {Automated Verification of Finite-trace Properties for Recursive Programs},
    school = {EECS, KTH Royal Institute of Technology},
    address = {Stockholm, Sweden},
    year = {2025},
}
```
