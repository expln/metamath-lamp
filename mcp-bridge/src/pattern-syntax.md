# Syntax of search patterns

## What a pattern is

For each assertion, the search engine creates a sequence of math symbols
by concatenating all essential hypotheses (in the order of definition) and the assertion statement.
A pattern defines a sequence of math symbols which must be found within that sequence.
If the pattern's sequence of symbols is a subsequence of the assertion's sequence,
then the assertion matches the pattern.

## The simplest pattern

A non-empty space-separated sequence of math symbols is a valid pattern.

Each constant in the pattern matches the same constant in an assertion.
If a constant is a typecode, then it matches any variable of that type.
For example, `class` matches any class variable and `wff` matches any wff variable.

A variable in the pattern matches a variable of the same type in an assertion
(but not necessarily of the same name).

If the same variable occurs two or more times in a pattern,
each occurrence matches the same variable in an assertion.
For example, if the first occurrence of `A` in a pattern matched `K` in an assertion,
then the second occurrence of `A` in the pattern also matches `K`.

Two different variables in a pattern match two different variables in an assertion.
For example, if a pattern is `A = B` and `A` matched `K`, then `B` cannot match `K`.

## Adjacent and non-adjacent symbols

By default, an arbitrary number of the assertion's symbols may appear between the pattern's symbols.
For example, `A + B + C` matches `( A + ( B + C ) )`, and `ph -> ps` matches `( ph -> A. x ps )`.

To disallow intervening symbols, surround the pattern symbols with `$[+` and `$]`.
For example, `$[+ ph -> ps $]` doesn't match `( ph -> A. x ps )`.

All control symbols of a pattern begin with the dollar sign `$`,
because this is the only character which cannot occur in math symbols.
All symbols, including control symbols, must be separated by whitespace.

The characters immediately following the opening bracket `$[` are called flags.
They specify a property of all the symbols within this pair of brackets.
The `+` flag means that the symbols must be adjacent, unless this is cancelled by the `-` flag (see below).

## The scope of pattern symbols

Non-adjacent symbols may be scattered across different hypotheses and the assertion statement,
which often produces useless results.
For example, `A + B = B + A` may match an assertion
whose hypothesis contains `A + B` and whose assertion statement contains `= B + A`.

To avoid this, specify in which part of an assertion the pattern symbols must occur, using these flags:

- `h` - any single hypothesis statement.
- `H` - the concatenation of all hypothesis statements (in the order of definition).
- `a` - the assertion statement.
- `s` - any single statement (a hypothesis or the assertion statement).

For example, `$[a A + B = B + A $]` requires all the symbols to occur in the assertion statement.

Flags which don't contradict each other can be combined, and their order doesn't matter.
For example, `$[+a ph -> ps $]` and `$[a+ ph -> ps $]` both find assertions
which have the adjacent symbols `ph -> ps` in the assertion statement.

Note that `$[+ha ph -> ps $]` requires `ph -> ps` to occur in a hypothesis and in the assertion at the same time,
which is impossible.
To find `ph -> ps` in either a hypothesis or the assertion, use the `s` flag: `$[+s ph -> ps $]`.

`Hs` is equivalent to `h`.

## Operators

Two or more sequences of symbols can be combined with operators:

- `$*` - the "ordered" operator.
  The operands must occur in the given order, possibly with other symbols in between.
  For example, `$[+ A + B $] $* = $* $[+ B + A $]` finds assertions which have adjacent symbols `A + B`,
  followed by `=`, followed by adjacent symbols `B + A`.
- `$/` - the "unordered" operator.
  It is like `$*`, but the operands may occur in any order.
  For example, `$[+ -. B < A $] $/ <-> $/ $[+ A <_ B $]` finds both `( A <_ B <-> -. B < A )`
  and `( -. B < A <-> A <_ B )`.
- `$|` - the "one of" operator.
  It matches if any of its operands matches.
  For example, `(/) $| C_` finds assertions which contain `(/)` or `C_`.

Different operators can be used in the same pattern.
For example, the pattern below finds assertions which may mean "`A. x ph` implies `A. x ps`":
`A. x ps` must be in the assertion statement,
and `A. x ph` must be either in a hypothesis,
or in the assertion statement followed by `->` or `<->`.

```
$[
    $[h+ A. x ph $]
    $|
    $[a
        $[+ A. x ph $]
        $*
        $[ -> $| <-> $]
    $]
$]
$*
$[a+ A. x ps $]
```

A pattern may span multiple lines.

## Using fewer brackets

A flag may be put at the beginning of a pattern after a single `$`.
Then the flag applies to the entire pattern, unless it is overridden by another flag.
For example:

- `$+ ph -> ps` is equivalent to `$[+ ph -> ps $]`.
- `$a+ ph -> ps` is equivalent to `$[a+ ph -> ps $]`.

Operands of operators don't need to be wrapped in brackets:

- `A + B $* = $* B + A` is equivalent to `$[ A + B $] $* = $* $[ B + A $]`.
- `$+ -. B < A $/ <-> $/ A <_ B` is equivalent to `$[+ -. B < A $] $/ <-> $/ $[+ A <_ B $]`.

Precedence of operators, from the highest to the lowest: `$*`, `$/`, `$|`.
For example:

- `A B $* C D $/ E F $* G H` is equivalent to `$[ A B $* C D $] $/ $[ E F $* G H $]`.
- `A B $/ C D $| E F $/ G H` is equivalent to `$[ A B $/ C D $] $| $[ E F $/ G H $]`.
- `A B $| C D $/ E F $* G H` is equivalent to `A B $| $[ C D $/ $[ E F $* G H $] $]`.

## Overriding flags

The `-` flag cancels the `+` flag inherited from an outer level.
For example, `$+ A B C $* $[- D E F $] $* G H I` is equivalent to `$[+ A B C $] $* D E F $* $[+ G H I $]`.

Using all the simplifications above, the "`A. x ph` implies `A. x ps`" example can be written as:

```
$+
$[
    $[h A. x ph $]
    $|
    $[a
        A. x ph
        $*
        $[ -> $| <-> $]
    $]
$]
$*
$[a A. x ps $]
```

## Multiple patterns

Several patterns may be given one after another, separated by whitespace.
An assertion must match all of them to be found.

A variable in one pattern is independent of a variable with the same name in another pattern.
For example, `$+h x = y $+a y = z` is not the same as `$+ $[h x = y $] $* $[a y = z $]`,
because `$+h x = y` and `$+a y = z` are two independent patterns,
so `y` in the first one and `y` in the second one may match different variables.

## Negation

The `!` flag negates a pattern, and it can be put only at the beginning of a pattern.
An assertion matches `$! <pattern>` if it doesn't match `<pattern>`.

For example, `$+ A + B $* = $* B + A $! ph` finds assertions about commutativity of addition
which don't contain any wff variables (the pattern variable `ph` matches any wff variable).
`$! wff` can be used instead of `$! ph`, because a typecode matches all variables of that type.
