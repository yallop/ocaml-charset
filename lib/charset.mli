(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Set.S with type elt = char

val to_rev_seq : t -> elt Seq.t
