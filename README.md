# Doldrums

&ldquo;You can swim all day in the Sea of Knowledge and not get wet.&rdquo;

&ndash; Norton Juster, _The Phantom Tollbooth_

## About

Doldrums is a small, purely functional programming language with an emphasis on ease of top-to-bottom understanding. I've been meaning to play with a language like this ever since making [Pixll](https://github.com/mitchellvitez/raspi-lights). This is a great way for me to learn. It's probably not very useful for anything in practice.

The compiler is written in Haskell. Run `stack run test.dol` to see an example.

### Structure

I wrote the parsing using [Megaparsec](https://hackage.haskell.org/package/megaparsec).

The `run` function performs each stage of the compilation pipeline. In order, it parses a small prelude (written in Doldrums), reads an input file, parses, evaluates, and shows the program results.

## The Doldrums language

Doldrums is purely functional, which means that all values are immutable. It's also lazy, tiny, and pretty useless in the real world.

### Comments

```
-- Line comments look like this
```

```
/* Block comments
   look like this */
```

### Writing a program

A program is a list of functions. A function has a name, a list of arguments, and a body.

```
id x = x;
const x y = x;
```

You can define constants using a "function" with no arguments.
```
seven = 7;
```

Every program has a main function. This is what runs when the program starts.
```
main = const 6 7;
```

### The $ operator

Because it has the lowest precedence, you can use `$` to replace parentheses in certain situations, for cleaner code. For example, 

```
main = f (g (h x));
```

is equivalent to

```
main = f $ g $ h x;
```

### Typechecking

Doldrums has a very simple typechecking mechanism, to ensure that certain kinds of invalid programs aren't allowed. For example, this program will fail to typecheck:

```
main = 7 + "hello";
```

So will this one, since you can't apply literals:

```
main = 1 2 3;
```

However, the mechanism currently only infers types within combinators, not across them. See this program for an example:

```
func x = x + 7;
main = func "hello";
```

The list of types is short: `Bool`, `Int`, `Double`, `String`, and `Constr`. They are inferred purely from the usage of literals and the combinations of those usages.

### Let expressions

You can define variables to be used in an expression with `let`...`in`
```
let n = 0 in n
```

Multiple definitions should be separated by commas
```
let a = 1, b = 2, c = 3
in a * b * c
```

Because Doldrums is lazy, you can define your variables in any order
```
let x = z
  , y = 7
  , z = y
in
  x
```

### Operator Precedence

Higher numbers mean higher precedence. All operators are binary (they have both a left and a right hand side).

Precedence | Associativity | Operator
-----------|---------------|---------
6          | left          | _function application_
5          | right         | *
5          |               | *.
5          |               | /
5          |               | /.
4          | right         | +
4          |               | +.
4          |               | -
4          |               | -.
3          |               | ==
3          |               | !=
3          |               | >
3          |               | >=
3          |               | <
3          |               | <=
2          | right         | &&
1          | right         | \|\|
0          | right         | $

## How can I do this?

I'd recommend using Megaparsec or another parsing library to make that part easier to write. I got the Template Instantiation material (`src/Template.hs`) from [Implementing Functional Languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial). [Statically Typed Interpreters](https://www.youtube.com/watch?v=Ci2KF5hVuEs) was helpful when figuring out how to add typechecking. Some issues were debugged more quickly thanks to help from friends.
