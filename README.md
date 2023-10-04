# VC Generation

In this assignment, you will implement a verifier based on the weakest-
precondition/VCGen methodology as discussed in class. We will work with programs
that are written in a JavaScript like syntax (EcmaScript) and translate them
into our imperative language Nano.

## Docker

As an alternative to building the assignment normally, you may also run the
it in a docker container. Make sure you install docker before executing the
following steps. The following commands need to be executed while in the root
of this repository.

First, you'll have to build the image. This is a slow process and may take 10
minutes or so:

```sh
$ docker build -t vc-gen .
```

Second, run the docker image. This will open up a shell, in which you can
compile, run and test the assignment via the normal commands.

```sh
$ docker run --rm -v .:/app -ti vc-gen
```

## Install Z3

This assignment will use Z3 to solve logical formulas. In order to use Z3
from code, you will need to install the developer version of Z3 (not just the
executable). Since we're using [Haskell's Z3 bindings](https://hackage.haskell.org/package/z3),
we need to use a specific version of Z3, namely `4.8.x`. Any other version will
give you compilation errors.

### Package Manager (Linux)

Use your package manager to install the z3 developer version. Here's an example
for Ubuntu.

```sh
$ sudo apt install libz3-dev
```

Make sure that you install the correct version!

### Manually

Go to the [Z3 releases](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.17)
page and pick the release for your OS and architecture.

#### Binary

From this release, copy the `z3` binary into your path.
 - `/usr/local/bin`, for Linux, MacOS.
 - `\Windows\System32\bin`, for Windows.

Make sure you can now run the binary from any other directory. You may need
to open a new terminal for this to work. If this gives you a version number,
then proceed!

```sh
$ z3 --version
```

#### Headers

Move the include files (directory `include`) into a directory for include
files. For example `/usr/local/include`, for Linux and MacOS. You can pick any
directory for Windows. Then, modify the `stack.yaml` file in this directory and
add the path to `extra-include-dirs`. For example:

```yaml
extra-include-dirs: [/usr/local/include]
```

#### Library

Copy the library files (all remaining files in the `bin` directory) and copy
them to your library path. For example `/usr/local/lib` for Linux, MacOS, or any
directory for Windows. Again, you need to update your `stack.yaml` to add the
library path:

```yaml
extra-lib-dirs: [/usr/local/lib]
```

## Running and testing

This code again features a test bench, which you may run in the same fashion
with `stack`. The tests again aim to direct you through to code base of this
assignment and we strongly suggest you follow this!

This assignment also features an executable which may be ran with the following
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
as it is to proof good programs. As such, we feature a bunch of programs to
achieve both goals. Programs in the `programs/pos` directory should pass
verification, while programs in the `programs/neg` directory should fail
verification.

### Verification

You will have to verify all the tests in `programs/verify`. As you are probably
already aware of at this point, we need to provide invariants to prove properties
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
infrastructure. Do make sure your submission correctly executes on our online
environment.

If there are issues with the submission system, don't panic! We will handle this
on a case-by-case basis.

If your uploaded submission somehow fail tests that work locally, ping
us and we will have a look!

If the online environment suddenly fails to work moments before the deadline,
don't hesitate to send us your submission through different means (e.g. email).

## Plagiarism

We have a strict zero tolerance policy against plagiarism. Sadly, we find cases
every year... This is not fun for you, nor us. Please, refrain from copying 
and/or sharing your code with other groups.

Since this is a group assignment, we expect that most of you will work together
via Git. Do make sure to make your repository **private**! Sharing your code in
this manner is sadly still plagiarism, even if unintentional.