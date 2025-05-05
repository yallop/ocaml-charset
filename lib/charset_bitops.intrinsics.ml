(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

external popcnt : int -> (int [@untagged]) = "caml_int_popcnt"
  "caml_int_popcnt_tagged_to_untagged" [@@noalloc]

external clz : int -> (int [@untagged]) = "caml_int_clz"
  "caml_int_clz_tagged_to_untagged" [@@noalloc]

external ctz : int -> int = "caml_int_ctz"
  "caml_int_ctz_untagged_to_untagged" [@@untagged] [@@noalloc]

(* Check that the signatures are as expected, since we're re-exporting externals *)
[@@@ocaml.warning "-32"]
module Int_ = Ocaml_intrinsics.Int
