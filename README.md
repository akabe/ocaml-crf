OCaml-CRF
=========

OCaml-CRF is a simple library for conditional random field in
[OCaml](http://ocaml.org/).

*Conditional random field* (CRF) is a graphical model widely used in natural language
processing, image processing, bioinformatics, etc.
This library provides several functions for *cyclic* CRF:

- Approximation of conditional probabilities, likelihood, and
  L2-regularized posterior by Gibbs sampling
- Inference of output labels by simulated annealing

Install
-------

### Dependencies

OCaml-CRF depends on [SLAP](http://akabe.github.io/slap/) and
[GSL-OCaml](https://github.com/mmottl/gsl-ocaml).

```
$ opam install slap gsl
```

### Install OCaml-CRF

```
$ ./configure
$ make
$ make install
```
