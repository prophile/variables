variables 0.1.2
===============

There have been a number of "monads-with-variables" implementations
on Hackage. They generally involve deep magic like type families
for the "variable type" of a particular monad.

The approach here is just to make the "variable type" for a monad
`m` the type `Variable m`, which is essentially a tuple of a getter
and a setter.

[![Build Status](https://travis-ci.org/prophile/variables.png?branch=master)](https://travis-ci.org/prophile/variables)

