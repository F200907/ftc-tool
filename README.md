# Finite-trace Tool

**This implementation serves as a proof-of-concept and is not maintained.**

The companion repository of the theoretical framework outlined in my master thesis.

- [Finite-trace Tool](#finite-trace-tool)
  - [Building](#building)
  - [Usage](#usage)
    - [Syntax](#syntax)
    - [Output](#output)
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

The problem should be formatted according to the following template.
Each building block's syntax is specified below.
`SPECIFICATION` is a trace formula, `PRECONDITION` and `POSTCONDITION` are boolean expressions, `m` is a procedure name with `S_m` its statement body, and `S` is the statement of the program.
The quotes surrounding the specification are necessary, they delimit the trace formula.
There can be arbitrarily many procedures in a given problem instance (denoted by `...`).
If no statement `S` is given, no verification problem is supplied.

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

The input's syntax is defined below in BNF-grammars with `x` being variable identifiers, `n` natural numbers, and `r` recursion variable identifiers.
Legal variable identifiers are strings starting in a letter followed by arbitrarily many alpha-numeric characters.

**Arithmetic expressions:**

```grammar
a ::= x | #x | n
  | -a | a + a | a - a | a * a
```

The generating rule `#x` is used to denote old variables.
This only has an effect in postconditions, in any other case the old variable `#x` is mapped to `x`.

**Boolean expressions:**

```grammar
b ::= true | false
  | a < a | a <= a | a > a | a >= a | a = a | a != a 
  | !b | b & b | (b | b)
```

Internally, the only implemented comparisons are `a < a` and `a = a`.
The others are expressed as conjunctions with negations.
This may lead to discrepancies in the output compared to the input.

**Trace formulas:**

```grammar
tf ::= b | Id | Sb_x^a | X_r 
   | tf && tf | tf || tf | tf ~ tf | \X_r.tf
```

Recursion variables are always prefixed by `X_` to distinguish them from state predicates.

**Rec statements:**

```grammar
s ::= skip | x := a | s; s | if b then s else s | m()
```

The if-then-else statement has a low precedence, therefore the input string `if b then s1 else s2; s3` is parsed as `if b then (s1) else (s2; s3)`.
To obtain the complementary parsing result, the input should be `(if b then s1 else s2); s3`.

### Output

The tool's output consists of the parsed input, the normalised input, whether contracts were verified, and whether the program adheres to the specification.
In the case that any of the verification steps (contracts or program) fails, a counter example is printed, that is, a state sequence which fails to satisfy the specification.
If any verification step outputs a counter example, then the program should be interpreted as not satisfying the specification according to the theory.

The final output does not take the contract's verification into account.
This is to allow for modularly verifying the problem's components.

The following in example output.

```sh
> trace-tool-exe -f "examples/ftc/prf.rec" -p

Parsed trace formula:
⊤ ⌢ μX_m.(Id ⌢ (Sb_x^0 ∨ Sb_x^(x - 1) ⌢ Id ⌢ X_m)) ⌢ Id
Normalised trace formula:
(⊤ ⌢ μX_m.((Id ⌢ Sb_x^0 ∨ Id ⌢ Sb_x^(x - 1) ⌢ Id ⌢ X_m)) ⌢ Id ∨ ⊤ ⌢ μX_$0.((⊤ ∨ ⊤ ⌢ X_$0)) ⌢ μX_m.((Id ⌢ Sb_x^0 ∨ Id ⌢ Sb_x^(x - 1) ⌢ Id ⌢ X_m)) ⌢ Id)
Parsed program:
[⊤]
[((x = 0) ∧ (z = #z))]
m {
(if ((x < 0) ∨ (x = 0)) then
  x := 0
else
  x := (x - 1);
  m())
}

z := 0;
y := x;
m();
y := ((y - x) + z)
Normalised program:
[⊤]
[((x = 0) ∧ (z = #z))]
m {
(if ((x < 0) ∨ (x = 0)) then
  x := 0
else
  x := (x - 1);
  m())
}

z := 0;
y := x;
m();
y := ((y - x) + z)

Checking for the contract of m:
Valid

Valid
```

### Known Issues

- Parsing an arithmetic expression may require an additional set of parenthesis, e.g., `Sb_x^0` raises an error whereas `Sb_x^(0)` is parsed correctly.
- A trace induced by procedure call violating the specification is not reflected in the counter example.

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
