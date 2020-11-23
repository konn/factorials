# factorials - Fast factorial functions in Haskell
![Haskell CI](https://github.com/konn/factorials/workflows/Haskell%20CI/badge.svg)

A Haskell implementation of _Divide, Swing and Conquer the Factorial!_ algorithms, an efficient integer factorioal function, as described in [FFF].

- `Math.NumberTheory.Factorial.Naive`: naive implementations using lists and acuumulated strict loop.
- `Math.NumberTheory.Factorial.Swing.Factorisation`: PrimeSwing method, using Swing numbers and factorisation.
- `Math.NumberTheory.Factorial.Swing.Recursion`: SplitRecursive method, using swing numbers and recursion, no factorisation.

[FFF]: http://www.luschny.de/math/factorial/FastFactorialFunctions.htm

