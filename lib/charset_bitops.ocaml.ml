(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

external (.!()) : 'a array -> int -> 'a = "%array_unsafe_get"

let rec clz_ r v =
  match v lsr 1 with
  | 0 -> r
  | v -> clz_ (r+1) v

let clz v = 62 - clz_ 0 v

let rec ctz_ r v =
  match v land 1 with
  | 1 -> r
  | _ -> ctz_ (r+1) (v lsr 1)

let ctz v = ctz_ 0 v

let bitcount = [|
    0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4;
    1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5;
    1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5;
    2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
    1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5;
    2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
    2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
    3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7;
    1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5;
    2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
    2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
    3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7;
    2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6;
    3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7;
    3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7;
    4; 5; 5; 6; 5; 6; 6; 7; 5; 6; 6; 7; 6; 7; 7; 8;
  |]

let popcnt n =
  bitcount.!((n lsr 0x00) land 0xff) +
+ bitcount.!((n lsr 0x08) land 0xff) +
+ bitcount.!((n lsr 0x10) land 0xff) +
+ bitcount.!((n lsr 0x18) land 0xff) +
+ bitcount.!((n lsr 0x20) land 0xff) +
+ bitcount.!((n lsr 0x28) land 0xff) +
+ bitcount.!((n lsr 0x30) land 0xff) +
+ bitcount.!((n lsr 0x38) land 0x7f)
                   
