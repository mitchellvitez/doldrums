# Doldrums

>You can swim all day in the Sea of Knowledge and not get wet.
--- Norton Juster

## About the project

- What?
  - Doldrums is a small, purely functional programming language with an emphasis on ease of top-to-bottom understanding.
- Why?
  - I've been meaning to play with a more involved language ever since making [Pixll](https://github.com/mitchellvitez/raspi-lights). This is a great way for me to learn. It's probably not very useful for anything in practice.
- Who?
  - Mitchell Vitez
- When?
  - Summer 2020
- How?
  - The compiler is written in Haskell. Run `stack run test.dol` to see an example.

## The Doldrums language

### Operator Precedence

Higher numbers mean higher precedence. All operators are binary (they have both a left and a right hand side).

Precedence | Associativity | Operators
-----------|---------------|---------
6          | left          | _function application_
5          | right         | *
5          |               | /
4          | right         | +
4          |               | -
3          |               | ==
3          |               | !=
3          |               | >
3          |               | >=
3          |               | <
3          |               | <=
2          | right         | &
1          | right         | |

### How can I do this?

I learned this material from [Implementing Functional Languages: a tutorial](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial) with help from a few friends.
