# VC Generation

In this assignment, you will implement a verifier based on the weakest-
precondition/VCGen methodology as discussed in class. We will work with programs
that are written in a JavaScript like syntax (EcmaScript) and translate them
into our imperative language Nano.

## Getting Started

Make sure you completed the [setup guide](https://github.com/Verification-for-Security/setup-guide).
Afterwards, set the following toolchain versions through the GHCup TUI.

- `GHC 9.4.8`
- `HLS 2.9.0.1`

### Install Z3

This assignment will use Z3 to solve logical formulas. Haskell's Z3 bindings
require us to use version `4.8.17` of Z3. Other version will likely not be
compatible. Stack should be able to automatically link against the library
if we place it in a folder called `z3` at the root of this assignment. To be
explicit, stack expects both the `z3/include` and `z3/bin` to be present, as it
will search these when attempting to link against Z3. The following command does
this install for you.

```sh
# Linux (and WSL)
wget -O z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.8.17/z3-4.8.17-x64-glibc-2.31.zip && unzip z3 && rm z3.zip && mv z3-4.8.17-* z3
```
```sh
# MacOS (ARM)
wget -O z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.8.17/z3-4.8.17-arm64-osx-10.16.zip && unzip z3 && rm z3.zip && mv z3-4.8.17-* z3 && ln -s z3/bin/libz3.dylib libz3.dylib
```

```sh
# MacOS (x64)
wget -O z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.8.17/z3-4.8.17-x64-osx-10.16.zip && unzip z3 && rm z3.zip && mv z3-4.8.17-* z3 && ln -s z3/bin/libz3.dylib libz3.dylib
```

Although we still highly recommend using WSL, for those using Windows directly,
you will have to install (and rename) Z3 manually from the [download page](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.17) such that it matches the
description above. When running the assignment, you additionally make the z3.dll
file visible in your path.

## Running and testing

This code again features a test bench, which you may run in the same fashion
with `stack`. The tests again aim to direct you through to code base of this
assignment and we strongly suggest you follow this!

```
$ stack test
```

This assignment also features an executable which you can run with the following
command.

```
$ stack run -- -f <path-to-file>
```

This will run the verifier on a single file. Again, you are allowed to make
modification to `Main` if you wish to get more debug information.

## Assignment

The assignment consists of two main parts. First you will write a weakest-
precondition verifier. After this, you will use this verifier to prove
properties about a set of small programs.

### Nano DSL

To get a grasp of what Nano DSL looks like, you can look at any of the programs
in the `programs` folder. One of the main things to note is that we interpret
some function calls as Nano statements.

Statement `assume(F)` encodes an assumption that formula F holds. 
Statement `assert(F)` encodes an obligation to show that formula F holds. 
In particular a Hoare triple {P} s {Q} can be translated into the following
expression.

```js
assume(P); s; assert(Q)
```

Statements `invariant(P)` places the given invariant `P` as (part of) the
invariant of the closest scoping while.

Statements `requires(P)` and `ensures(P)` respecitvely set a pre or post 
condition on the function signature.

Statements `modifies(x)` says that the inner variable will be modified by
the function call.

Expressions `forall(x, P)` and `exists(x, P)` quantify `x` over `P` and may
be used wherever logic is involved (i.e. all of these prior mentioned calls).

**Aside:** Embedding a new language (Nano) into an existing language
(Haskell) is a common and important [technique](http://wiki.haskell.org/Embedded_domain_specific_language)
in programming language research. The embedded language is often called a Domain
Specific Language (DSL). We're using an approached called `deep embedding`.

### Verifier

To get a working verifier, you will have to implement roughly the following
steps.

1. Substition of logical formulas
2. Conversion of ECMAScript into Nano
3. Generation of Verification Conditions from a Nano program

Regarding the ECMAScript conversion, you may find the documentation of the
parser [here](https://hackage.haskell.org/package/language-ecmascript-0.17.0.1/docs/Language-ECMAScript3-Syntax.html).
Make sure to check out the nano programs to form a better understanding of what
the embedding looks like.

For your verifier, it is just as important that your code rejects bad programs
as it is to prove good programs. As such, we feature a bunch of programs to
achieve both goals. Programs in the `programs/pos` directory should pass
verification, while programs in the `programs/neg` directory should fail
verification.

### Verification

You will have to verify all the tests in `programs/verify`. 
As you know, we need to provide invariants to prove properties
when loops are involved.

You may add invariants to the files in this folder in order to verify them.
We do check that you do not modify the code in any other way.

**Tip** Instead of writing 

```js
invariant(P && Q && R)
```

you can write 

```js
invariant(P);
invariant(Q);
invariant(R);
```

## Grading

Your final grade corresponds directly to the one awarded to you by the test
infrastructure. Make sure your submission is correctly executed in our online
environment.

If there are issues with the submission system, don't panic! We will handle this
on a case-by-case basis.

If your uploaded submission somehow fail tests that work locally, ping
us and we will have a look!

If the online environment suddenly fails to work moments before the deadline,
don't hesitate to send us your submission through different means (e.g. email).

## Plagiarism

We have a strict zero-tolerance policy against plagiarism. Please, refrain from copying 
and/or sharing your code with other groups.

Since this is a group assignment, we expect that most of you will work together
via Git. Do make sure to make your repository **private**! Sharing your code in
this manner is plagiarism, even if unintentional.
