## Charset

[![Charset](https://github.com/yallop/ocaml-charset/actions/workflows/test.yml/badge.svg)](https://github.com/yallop/ocaml-charset/actions/workflows/test.yml)

This library provides a fast drop-in replacement for the standard library's `Set.Make(Char)`, implemented as a bit array.  All functions follow the documented behaviour of the corresponding standard library functions.

All pure set operations (i.e. functions on sets and elements, rather than on functions, lists, sequences, etc.) are intended to execute consistently in around 10ns or less, regardless of set size:

```ocaml
val union : t -> t -> t
```
![union][union]


```ocaml
val equal : t -> t -> bool
```
![equal][equal]

Other operations are competitive with the corresponding standard library implementations:

```ocaml
val of_list : elt list -> t
```
![of-list][of-list]

```ocaml
val iter : (elt -> unit) -> t -> unit
```
![iter][iter]

See [BENCHMARKS.md](https://github.com/yallop/ocaml-charset/blob/master/BENCHMARKS.md) for a full set of benchmark results.

--------------------------------------------------------------------------------

[union]: images/union-benchmark.svg
[iter]: images/iter-benchmark.svg
[equal]: images/equal-benchmark.svg
[of-list]: images/of-list-benchmark.svg

