(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let sizes = [0;1;2;4;8;16;32;64;96;128;160;192;224;256]

module type S = module type of Charset

module Common(X: S) =
struct
  let all = X.of_list (List.init 256 Stdlib.Char.chr)

  let sets n = 
    let set1, set2 = X.partition (fun _ -> Random.int 256 < n) all in
    let set3, set4 = X.partition (fun _ -> Random.int 256 < n) all in
    set1, set2, set3, set4

  let min_elt n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    match X.min_elt set1 with
    | exception Not_found -> ()
    | _ -> ()

  let min_elt_opt n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.min_elt_opt set1)

  let choose n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    match X.choose set1 with
    | exception Not_found -> ()
    | _ -> ()

  let choose_opt n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.choose_opt set1)

  let max_elt n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    match X.max_elt set1 with
    | exception Not_found -> ()
    | _ -> ()

  let max_elt_opt n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.max_elt_opt set1)

  let union n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.union set1 set3)

  let inter n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.inter set1 set3)

  let diff n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.diff set1 set3)

  let compare n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.compare set1 set3)

  let equal n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.equal set1 set3)

  let cardinal n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.cardinal set1)

  let subset n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.subset set1 set3)

  let disjoint n =
    let (set1,_,set3,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.disjoint set1 set3)

  let for_all n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.for_all (fun _ -> true) set1)

  let exists n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.exists (fun _ -> false) set1)

  let filter n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.filter (fun c -> Char.lowercase_ascii c <> c) set1)

  let filter_map n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.filter_map Option.some set1)

  let elements n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.elements set1)

  let of_list n =
    let (set1,_,_,_) = sets n in
    let elements = X.elements set1 in
    Core.Staged.stage @@ fun () ->
    ignore (X.of_list elements)

  let fastmap n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.map Fun.id set1)

  let slowmap n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.map (fun c -> Char.unsafe_chr (256 - Char.code c)) set1)

  let fold n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.fold List.cons set1 [])

  let iter n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.iter ignore set1)

  let is_empty n =
    let (set1,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.is_empty set1)

  let mem n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.mem 'a' set)

  let add n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.add 'a' set)

  let remove n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.remove 'a' set)

  let find n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    match X.find 'a' set with
    | exception Not_found -> ()
    | _ -> ()

  let find_opt n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.find_opt 'a' set)

  let singleton _ = (* TODO: this shouldn't be parameterized *)
    Core.Staged.stage @@ fun () ->
    ignore (X.singleton 'a')

  let split n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.split 'a' set)

  let of_seq n =
    let (set,_,_,_) = sets n in
    let seq = List.to_seq (X.elements set) in
    Core.Staged.stage @@ fun () ->
    ignore (X.of_seq seq)

  let add_seq n =
    let (set1,_,set3,_) = sets n in
    let seq = List.to_seq (X.elements set1) in
    Core.Staged.stage @@ fun () ->
    ignore (X.add_seq seq set3)

  let to_seq n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    Seq.iter ignore (X.to_seq set)

  let to_rev_seq n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    Seq.iter ignore (X.to_rev_seq set)

  let to_seq_from n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    Seq.iter ignore (X.to_seq_from 'a' set)

  let find_first n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    match X.find_first ((<=)'\127') set with
    | exception Not_found -> ()
    | _ -> ()

  let find_first_opt n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.find_first_opt ((<=)'\127') set)

  let find_last n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    match X.find_last ((>=)'\127') set with
    | exception Not_found -> ()
    | _ -> ()

  let find_last_opt n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.find_last_opt ((>=)'\127') set)

  let partition n =
    let (set,_,_,_) = sets n in
    Core.Staged.stage @@ fun () ->
    ignore (X.partition ((>=)'\127') set)
end

module Stdlib_CharSet =
struct
  module C = Stdlib.Set.Make(Char)
  (* inefficient version for pre-4.12 compatibility *)
  let[@ocaml.warning "-32"] to_rev_seq s = List.to_seq (List.rev (C.elements s))
  include C
end
module Stdtests = Common(Stdlib_CharSet)
module Ourtests = Common(Charset)

open Core_bench

let group name f g =
  (name,
   Bench.make_command [
       Bench.Test.create_indexed ~name:("stdlib_"^ name) ~args:sizes f;
       Bench.Test.create_indexed ~name:("charset_"^ name) ~args:sizes g;
  ])

let () =
  Core.Command.run
    (Core.Command.group ~summary:"char benchmarks"
       [group "isempty" Stdtests.is_empty Ourtests.is_empty;
        group "mem" Stdtests.mem Ourtests.mem;
        group "add" Stdtests.add Ourtests.add;
        group "singleton" Stdtests.singleton Ourtests.singleton;
        group "remove" Stdtests.remove Ourtests.remove;
        group "union" Stdtests.union Ourtests.union;
        group "inter" Stdtests.inter Ourtests.inter;
        group "disjoint" Stdtests.disjoint Ourtests.disjoint;
        group "diff" Stdtests.diff Ourtests.diff;
        group "compare" Stdtests.compare Ourtests.compare;
        group "equal" Stdtests.equal Ourtests.equal;
        group "subset" Stdtests.subset Ourtests.subset;
        group "iter" Stdtests.iter Ourtests.iter;
        group "fastmap" Stdtests.fastmap Ourtests.fastmap;
        group "slowmap" Stdtests.slowmap Ourtests.slowmap;
        group "fold" Stdtests.fold Ourtests.fold;
        group "forall" Stdtests.for_all Ourtests.for_all;
        group "exists" Stdtests.exists Ourtests.exists;
        group "filter" Stdtests.filter Ourtests.filter;
        group "filter-map" Stdtests.filter_map Ourtests.filter_map;
        group "partition" Stdtests.partition Ourtests.partition;
        group "cardinal" Stdtests.cardinal Ourtests.cardinal;
        group "elements" Stdtests.elements Ourtests.elements;
        group "minelt" Stdtests.min_elt Ourtests.min_elt;
        group "minelt-opt" Stdtests.min_elt_opt Ourtests.min_elt_opt;
        group "maxelt" Stdtests.max_elt Ourtests.max_elt;
        group "maxelt-opt" Stdtests.max_elt_opt Ourtests.max_elt_opt;
        group "choose" Stdtests.choose Ourtests.choose;
        group "choose-opt" Stdtests.choose_opt Ourtests.choose_opt;
        group "split" Stdtests.split Ourtests.split;
        group "find" Stdtests.find Ourtests.find;
        group "find-opt" Stdtests.find_opt Ourtests.find_opt;
        group "find-first" Stdtests.find_first Ourtests.find_first;
        group "find-first-opt" Stdtests.find_first_opt Ourtests.find_first_opt;
        group "find-last" Stdtests.find_last Ourtests.find_last;
        group "find-last-opt" Stdtests.find_last_opt Ourtests.find_last_opt;
        group "of-list" Stdtests.of_list Ourtests.of_list;
        group "to-seq-from" Stdtests.to_seq_from Ourtests.to_seq_from;
        group "to-seq" Stdtests.to_seq Ourtests.to_seq;
        group "to-rev-seq" Stdtests.to_rev_seq Ourtests.to_rev_seq;
        group "add-seq" Stdtests.add_seq Ourtests.add_seq;
        group "of-seq" Stdtests.of_seq Ourtests.of_seq;
    ])
