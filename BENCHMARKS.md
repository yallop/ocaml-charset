## Benchmarks for [Charset](https://github.com/yallop/ocaml-charset)

All pure set operations (i.e. functions on sets and elements, rather than on functions, lists, sequences, etc.) are intended to execute consistently in around 10ns or less, regardless of set size.

Other operations are competitive with the corresponding standard library implementations.  (The functions `find-first`, `find-first-opt`, `find-last` and `find-last-opt` are currently an exception; they are significantly slower than the standard library implementations.)

#### pure set operations (should be <~10ns)

```ocaml
val is_empty : t -> bool
```
![isempty][isempty]

```ocaml
val mem : elt -> t -> bool
```
![mem][mem]

```ocaml
val add : elt -> t -> t       val remove : elt -> t -> t
```
![add][add] ![remove][remove]

```ocaml
val singleton : elt -> t
```
![singleton][singleton]


```ocaml
val union : t -> t -> t       val inter : t -> t -> t
```
![union][union] ![inter][inter]

```ocaml
val disjoint : t -> t -> bool
```
![disjoint][disjoint]


```ocaml
val diff : t -> t -> t
```
![diff][diff]


```ocaml
val compare : t -> t -> int   val equal : t -> t -> bool
```
![compare][compare] ![equal][equal]

```ocaml
val subset : t -> t -> bool
```
![subset][subset]

```ocaml
val cardinal : t -> int
```
![cardinal][cardinal]


```ocaml
val min_elt : t -> elt        val min_elt_opt : t -> elt option
```
![minelt][minelt] ![minelt-opt][minelt-opt]

```ocaml
val max_elt : t -> elt        val max_elt_opt : t -> elt option
```
![maxelt][maxelt] ![maxelt-opt][maxelt-opt]

```ocaml
val choose : t -> elt         val choose_opt : t -> elt option
```
![choose][choose] ![choose-opt][choose-opt]

```ocaml
val find : elt -> t -> elt    val find_opt : elt -> t -> elt option
```
![find][find] ![find-opt][find-opt]

```ocaml
val split : elt -> t -> t * bool * t
```
![split][split]

#### other operations, which use functions, sequences, lists, etc. (should be competitive with the standard library)

```ocaml
val iter : (elt -> unit) -> t -> unit
```
![iter][iter]


```ocaml
val map : (elt -> elt) -> t -> t
```
![fastmap][fastmap] ![slowmap][slowmap]

```ocaml
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
```
![fold][fold]

```ocaml
val exists : (elt -> bool) -> t -> bool     val for_all : (elt -> bool) -> t -> bool
```
![exists][exists] ![forall][forall]


```ocaml
val filter : (elt -> bool) -> t -> t        val filter_map : (elt -> elt option) -> t -> t
```
![filter][filter] ![filter-map][filter-map]


```ocaml
val partition : (elt -> bool) -> t -> t * t
```
![partition][partition]

```ocaml
val elements : t -> elt list
```
![elements][elements]


```ocaml
val of_list : elt list -> t
```
![of-list][of-list]

```ocaml
val to_seq_from : elt -> t -> elt Seq.t
```
![to-seq-from][to-seq-from]

```ocaml
val to_seq : t -> elt Seq.t
```
![to-seq][to-seq]

```ocaml
val to_rev_seq : t -> elt Seq.t
```
![to-rev-seq][to-rev-seq]

```ocaml
val add_seq : elt Seq.t -> t -> t
```
![add-seq][add-seq]


```ocaml
val of_seq : elt Seq.t -> t
```
![of-seq][of-seq]

#### operations that currently have inefficient implementations

```ocaml
val find_first : (elt -> bool) -> t -> elt    val find_first_opt : (elt -> bool) -> t -> elt option
```
![find-first][find-first] ![find-first-opt][find-first-opt]

```ocaml
val find_last : (elt -> bool) -> t -> elt     val find_last_opt : (elt -> bool) -> t -> elt option
```
![find-last][find-last] ![find-last-opt][find-last-opt]


--------------------------------------------------------------------------------

[cardinal]: images/cardinal-benchmark.svg
[disjoint]: images/disjoint-benchmark.svg
[exists]: images/exists-benchmark.svg
[forall]: images/forall-benchmark.svg
[minelt]: images/minelt-benchmark.svg
[maxelt]: images/maxelt-benchmark.svg
[union]: images/union-benchmark.svg
[compare]: images/compare-benchmark.svg
[elements]: images/elements-benchmark.svg
[fastmap]: images/fastmap-benchmark.svg
[inter]: images/inter-benchmark.svg
[slowmap]: images/slowmap-benchmark.svg
[diff]: images/diff-benchmark.svg
[equal]: images/equal-benchmark.svg
[filter]: images/filter-benchmark.svg
[isempty]: images/isempty-benchmark.svg
[subset]: images/subset-benchmark.svg
[add]: images/add-benchmark.svg
[find]: images/find-benchmark.svg
[mem]: images/mem-benchmark.svg
[remove]: images/remove-benchmark.svg
[find-opt]: images/find-opt-benchmark.svg
[maxelt-opt]: images/maxelt-opt-benchmark.svg
[minelt-opt]: images/minelt-opt-benchmark.svg
[choose]: images/choose-benchmark.svg
[choose-opt]: images/choose-opt-benchmark.svg
[singleton]: images/singleton-benchmark.svg
[split]: images/split-benchmark.svg
[iter]: images/iter-benchmark.svg
[add-seq]: images/add-seq-benchmark.svg
[filter-map]: images/filter-map-benchmark.svg
[of-seq]: images/of-seq-benchmark.svg
[to-rev-seq]: images/to-rev-seq-benchmark.svg
[to-seq]: images/to-seq-benchmark.svg
[to-seq-from]: images/to-seq-from-benchmark.svg
[find-first-opt]: images/find-first-opt-benchmark.svg
[find-last-opt]: images/find-last-opt-benchmark.svg
[find-first]: images/find-first-benchmark.svg
[find-last]: images/find-last-benchmark.svg
[fold]: images/fold-benchmark.svg
[of-list]: images/of-list-benchmark.svg
[partition]: images/partition-benchmark.svg
