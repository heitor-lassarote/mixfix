# A Haskell port of Parsing Mixfix Operators

This repository contains a simple Haskell port (and simplification) of the Agda code implemented in Nils Anders Danielsson's and Ulf Norell's [Parsing Mixfix Operators](https://www.researchgate.net/publication/221600485_Parsing_Mixfix_Operators).

Contrary to the Agda code, this does not contain all the parts of the formalization of the productions, but rather a simplification that is more practical to use and simpler to implement in Haskell, which has no dependent types yet.

* [`app`](./app) contains a very simple executable with a few predefined operators that read from the input and output the parsed AST.
* [`src`](./src) contains the actual implementation, as defined in the paper. That is, the port of the algorithm, a parser for it, and a simple pretty-printer.
* [`test`](./test) contains a few simple tests, which also replicate the operators in [`app/Main.hs`](./app/Main.hs).
