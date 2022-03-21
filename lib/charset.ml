(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Primitive types and operations from which we can fashion charsets *)

open Charset_bitops
external int_of_bool : bool -> int = "%identity"
external (.!()) : 'a array -> int -> 'a = "%array_unsafe_get"
external char : int -> char = "%identity"
let int : char -> int = Char.code


(* We'll assume >=63-bit ints for now.  We can generalize to other sizes later *)
let () = assert (Sys.int_size >= 63)
type t = int array (* 60, 60, 60, 60, 16 *)
type elt = char

(** Constructors and updates *)

let fresh () : t = Array.make 5 0

let clone = Array.copy

let singleton c =
  match c with
  | '\000'..'\059' -> [| 1             lsl (int c      ); 0; 0; 0; 0 |]
  | '\060'..'\119' -> [| 0; 1          lsl (int c -  60); 0; 0; 0 |]
  | '\120'..'\179' -> [| 0; 0; 1       lsl (int c - 120); 0; 0 |]
  | '\180'..'\239' -> [| 0; 0; 0; 1    lsl (int c - 180); 0 |]
  | _              -> [| 0; 0; 0; 0; 1 lsl (int c - 240) |]

let add c (s : t) =
  let v = s.!(0) and w = s.!(1) and x = s.!(2) and y = s.!(3) and z = s.!(4) in
  match c with
  | '\000'..'\059' -> [| v             lor (1 lsl (int c      )); w; x; y; z |]
  | '\060'..'\119' -> [| v; w          lor (1 lsl (int c -  60)); x; y; z |]
  | '\120'..'\179' -> [| v; w; x       lor (1 lsl (int c - 120)); y; z |]
  | '\180'..'\239' -> [| v; w; x; y    lor (1 lsl (int c - 180)); z |]
  | _              -> [| v; w; x; y; z lor (1 lsl (int c - 240)) |]

let remove c (s : t) =
  let v = s.!(0) and w = s.!(1) and x = s.!(2) and y = s.!(3) and z = s.!(4) in
  match c with
  | '\000'..'\059' -> [| v             land lnot (1 lsl (int c      )); w; x; y; z |]
  | '\060'..'\119' -> [| v; w          land lnot (1 lsl (int c -  60)); x; y; z |]
  | '\120'..'\179' -> [| v; w; x       land lnot (1 lsl (int c - 120)); y; z |]
  | '\180'..'\239' -> [| v; w; x; y    land lnot (1 lsl (int c - 180)); z |]
  | _              -> [| v; w; x; y; z land lnot (1 lsl (int c - 240)) |]


(** Random access *)

let get (s : t) c : bool =
  match c with
  | '\000'..'\059' -> ((s.!(0) lsr (int c      )) land 1) <> 0
  | '\060'..'\119' -> ((s.!(1) lsr (int c - 60 )) land 1) <> 0
  | '\120'..'\179' -> ((s.!(2) lsr (int c - 120)) land 1) <> 0
  | '\180'..'\239' -> ((s.!(3) lsr (int c - 180)) land 1) <> 0
  | _              -> ((s.!(4) lsr (int c - 240)) land 1) <> 0

let set (s : t) i =
  match i with
  | '\000'..'\059' -> Array.unsafe_set s 0 (s.!(0) lor (1 lsl (int i      )))
  | '\060'..'\119' -> Array.unsafe_set s 1 (s.!(1) lor (1 lsl (int i - 60 )))
  | '\120'..'\179' -> Array.unsafe_set s 2 (s.!(2) lor (1 lsl (int i - 120)))
  | '\180'..'\239' -> Array.unsafe_set s 3 (s.!(3) lor (1 lsl (int i - 180)))
  | _              -> Array.unsafe_set s 4 (s.!(4) lor (1 lsl (int i - 240)))


(** Iterators for linear scans.
    Valid iterators are chars in [0..255]; invalid iterators are -1 *)

let first4 (s : t) = match s.!(4) with 0 -> -1       | x -> 240 + ctz x
let first3 (s : t) = match s.!(3) with 0 -> first4 s | x -> 180 + ctz x
let first2 (s : t) = match s.!(2) with 0 -> first3 s | x -> 120 + ctz x
let first1 (s : t) = match s.!(1) with 0 -> first2 s | x ->  60 + ctz x
let first0 (s : t) = match s.!(0) with 0 -> first1 s | x ->       ctz x
let first = first0

let last0 (s : t) = match s.!(0) with 0 -> -1      | x ->  62 - clz x
let last1 (s : t) = match s.!(1) with 0 -> last0 s | x -> 122 - clz x
let last2 (s : t) = match s.!(2) with 0 -> last1 s | x -> 182 - clz x
let last3 (s : t) = match s.!(3) with 0 -> last2 s | x -> 242 - clz x
let last4 (s : t) = match s.!(4) with 0 -> last3 s | x -> 302 - clz x
let last = last4

let next (s : t) c = match c with
  | '\000'..'\059' -> (match s.!(0) land (lnot ((1 lsl (int c + 1)) - 1)) with
                       | 0 -> first1 s
                       | x -> ctz x)
  | '\060'..'\119' -> (match s.!(1) land (lnot ((1 lsl (int c - 59)) - 1)) with
                       | 0 -> first2 s
                       | x -> 60 + ctz x)
  | '\120'..'\179' -> (match s.!(2) land (lnot ((1 lsl (int c - 119)) - 1)) with
                       | 0 -> first3 s
                       | x -> 120 + ctz x)
  | '\180'..'\239' -> (match s.!(3) land (lnot ((1 lsl (int c - 179)) - 1)) with
                       | 0 -> first4 s
                       | x -> 180 + ctz x)
  | _              -> (match s.!(4) land (lnot ((1 lsl (int c - 239)) - 1)) with
                       | 0 -> -1
                       | x -> 240 + ctz x)

let prev (s : t) c = match c with
  | '\000'..'\059' -> (match s.!(0) land ((1 lsl (int c)) - 1) with
                       | 0 -> -1
                       | x -> 62 - clz x)
  | '\060'..'\119' -> (match s.!(1) land ((1 lsl (int c - 60)) - 1) with
                       | 0 -> last0 s
                       | x -> 122 - clz x)
  | '\120'..'\179' -> (match s.!(2) land ((1 lsl (int c - 120)) - 1) with
                       | 0 -> last1 s
                       | x -> 182 - clz x)
  | '\180'..'\239' -> (match s.!(3) land ((1 lsl (int c - 180)) - 1) with
                       | 0 -> last2 s
                       | x -> 242 - clz x)
  | _              -> (match s.!(4) land ((1 lsl (int c - 240)) - 1) with
                       | 0 -> last3 s
                       | x -> 302 - clz x)

(** Comparisons *)

let compare (l : t) (r : t) : int =
  let c0 = compare l.!(0) r.!(0) in
  if c0 <> 0 then c0 else
  let c1 = compare l.!(1) r.!(1) in
  if c1 <> 0 then c1 else
  let c2 = compare l.!(2) r.!(2) in
  if c2 <> 0 then c2 else
  let c3 = compare l.!(3) r.!(3) in
  if c3 <> 0 then c3 else
           compare l.!(4) r.!(4)

let equal (l : t) (r : t) = l.!(0) = r.!(0) &&
                            l.!(1) = r.!(1) &&
                            l.!(2) = r.!(2) &&
                            l.!(3) = r.!(3) &&
                            l.!(4) = r.!(4)

(** Aggregate operations *)

let is_empty (s : t) = s.!(0) = 0 &&
                       s.!(1) = 0 &&
                       s.!(2) = 0 &&
                       s.!(3) = 0 &&
                       s.!(4) = 0

let union l r = [| l.!(0) lor r.!(0);
                   l.!(1) lor r.!(1);
                   l.!(2) lor r.!(2);
                   l.!(3) lor r.!(3);
                   l.!(4) lor r.!(4); |]

let inter l r = [| l.!(0) land r.!(0);
                   l.!(1) land r.!(1);
                   l.!(2) land r.!(2);
                   l.!(3) land r.!(3);
                   l.!(4) land r.!(4); |]


let diff l r = [| l.!(0) land lnot (r.!(0));
                  l.!(1) land lnot (r.!(1));
                  l.!(2) land lnot (r.!(2));
                  l.!(3) land lnot (r.!(3));
                  l.!(4) land lnot (r.!(4)); |]

let cardinal s =
  popcnt s.!(0) + popcnt s.!(1) + popcnt s.!(2) + popcnt s.!(3) + popcnt s.!(4)

let disjoint l r =
  (l.!(0) land r.!(0)) = 0 &&
  (l.!(1) land r.!(1)) = 0 &&
  (l.!(2) land r.!(2)) = 0 &&
  (l.!(3) land r.!(3)) = 0 &&
  (l.!(4) land r.!(4)) = 0


(** Higher-order traversals *)

let combine4 a b c d = (int_of_bool a lsl 0x0) lor
                       (int_of_bool b lsl 0x1) lor
                       (int_of_bool c lsl 0x2) lor
                       (int_of_bool d lsl 0x3)

let iter_dbytes f offset v =
  let rec loop f offset v =
    if v <> 0 then begin
        f (char (ctz v + offset));
        loop f offset (v land pred v)
      end
  in loop f offset v

let iter (f : char -> unit) (s : t) : unit =
  begin
    iter_dbytes f 0   s.!(0);
    iter_dbytes f 60  s.!(1);
    iter_dbytes f 120 s.!(2);
    iter_dbytes f 180 s.!(3);
    iter_dbytes f 240 s.!(4); (* Slight overkill; some of these bits are always zero *)
  end

let exists (f : char -> bool) (s : t) : bool =
  let rec loop f offset v = v <> 0 && (f (char (ctz v + offset)) || loop f offset (v land pred v)) in
  loop f 0   s.!(0) ||
  loop f 60  s.!(1) ||
  loop f 120 s.!(2) ||
  loop f 180 s.!(3) ||
  loop f 240 s.!(4)

let for_all (f : char -> bool) (s : t) : bool =
  let rec loop f offset v = v = 0 || (f (char (ctz v + offset)) && loop f offset (v land pred v)) in
  loop f 0   s.!(0) &&
  loop f 60  s.!(1) &&
  loop f 120 s.!(2) &&
  loop f 180 s.!(3) &&
  loop f 240 s.!(4)

let fold (type a) (f : char -> a -> a) (s : t) (acc : a) : a =
  let bits f i nib acc =
    match nib with
    | 0b0000 ->                                                            acc
    | 0b0001 ->                                                 f (char i) acc
    | 0b0010 ->                                 f (char (i+1))             acc
    | 0b0011 ->                                 f (char (i+1)) (f (char i) acc)
    | 0b0100 ->                 f (char (i+2))                             acc
    | 0b0101 ->                 f (char (i+2))                 (f (char i) acc)
    | 0b0110 ->                 f (char (i+2)) (f (char (i+1))             acc)
    | 0b0111 ->                 f (char (i+2)) (f (char (i+1)) (f (char i) acc))
    | 0b1000 -> f (char (i+3))                                             acc
    | 0b1001 -> f (char (i+3))                                 (f (char i) acc)
    | 0b1010 -> f (char (i+3))                 (f (char (i+1))             acc)
    | 0b1011 -> f (char (i+3))                 (f (char (i+1)) (f (char i) acc))
    | 0b1100 -> f (char (i+3)) (f (char (i+2))                             acc)
    | 0b1101 -> f (char (i+3)) (f (char (i+2))                 (f (char i) acc))
    | 0b1110 -> f (char (i+3)) (f (char (i+2)) (f (char (i+1))             acc))
    | _      -> f (char (i+3)) (f (char (i+2)) (f (char (i+1)) (f (char i) acc)))
  in
  let nibs f i db acc =
    let b0 = (db lsr 0x0) land 0xf
    and b1 = (db lsr 0x4) land 0xf
    and b2 = (db lsr 0x8) land 0xf
    and b3 = (db lsr 0xc) land 0xf in
    match combine4 (b0 <> 0) (b1 <> 0) (b2 <> 0) (b3 <> 0) with
    | 0b0000 ->                                                                 acc
    | 0b0001 ->                                                     bits f i b0 acc
    | 0b0010 ->                                    bits f (i+4) b1              acc
    | 0b0011 ->                                    bits f (i+4) b1 (bits f i b0 acc)
    | 0b0100 ->                   bits f (i+8) b2                               acc
    | 0b0101 ->                   bits f (i+8) b2                  (bits f i b0 acc)
    | 0b0110 ->                   bits f (i+8) b2 (bits f (i+4) b1              acc)
    | 0b0111 ->                   bits f (i+8) b2 (bits f (i+4) b1 (bits f i b0 acc))
    | 0b1000 -> bits f (i+12) b3                                                acc
    | 0b1001 -> bits f (i+12) b3                                   (bits f i b0 acc)
    | 0b1010 -> bits f (i+12) b3                  (bits f (i+4) b1              acc)
    | 0b1011 -> bits f (i+12) b3                  (bits f (i+4) b1 (bits f i b0 acc))
    | 0b1100 -> bits f (i+12) b3 (bits f (i+8) b2                               acc)
    | 0b1101 -> bits f (i+12) b3 (bits f (i+8) b2                  (bits f i b0 acc))
    | 0b1110 -> bits f (i+12) b3 (bits f (i+8) b2 (bits f (i+4) b1              acc))
    | _      -> bits f (i+12) b3 (bits f (i+8) b2 (bits f (i+4) b1 (bits f i b0 acc)))
  in
  let dbytes f i dw acc =
    let n0 = (dw lsr 0x00) land 0xffff
    and n1 = (dw lsr 0x10) land 0xffff
    and n2 = (dw lsr 0x20) land 0xffff
    and n3 = (dw lsr 0x30) land 0xffff in
    match combine4 (n0 <> 0) (n1 <> 0) (n2 <> 0) (n3 <> 0) with
    | 0b0000 ->                                                                   acc
    | 0b0001 ->                                                       nibs f i n0 acc
    | 0b0010 ->                                     nibs f (i+16) n1              acc
    | 0b0011 ->                                     nibs f (i+16) n1 (nibs f i n0 acc)
    | 0b0100 ->                   nibs f (i+32) n2                                acc
    | 0b0101 ->                   nibs f (i+32) n2                   (nibs f i n0 acc)
    | 0b0110 ->                   nibs f (i+32) n2 (nibs f (i+16) n1              acc)
    | 0b0111 ->                   nibs f (i+32) n2 (nibs f (i+16) n1 (nibs f i n0 acc))
    | 0b1000 -> nibs f (i+48) n3                                                  acc
    | 0b1001 -> nibs f (i+48) n3                                     (nibs f i n0 acc)
    | 0b1010 -> nibs f (i+48) n3                   (nibs f (i+16) n1              acc)
    | 0b1011 -> nibs f (i+48) n3                   (nibs f (i+16) n1 (nibs f i n0 acc))
    | 0b1100 -> nibs f (i+48) n3 (nibs f (i+32) n2                                acc)
    | 0b1101 -> nibs f (i+48) n3 (nibs f (i+32) n2                   (nibs f i n0 acc))
    | 0b1110 -> nibs f (i+48) n3 (nibs f (i+32) n2 (nibs f (i+16) n1              acc))
    | _      -> nibs f (i+48) n3 (nibs f (i+32) n2 (nibs f (i+16) n1 (nibs f i n0 acc)))
  in
    (dbytes f 240   s.!(4) (* Slight overkill; some of these bits are always zero *)
       (dbytes f 180  s.!(3)
          (dbytes f 120 s.!(2)
             (dbytes f 60 s.!(1)
                (dbytes f 0 s.!(0) acc)))))


(** Misc *)

let elements (s : t) : char list =
  let bits i nib acc =
    match nib with
    | 0b0000 ->                                                     acc
    | 0b0001 -> char i ::                                           acc
    | 0b0010 ->           char (i+1) ::                             acc
    | 0b0011 -> char i :: char (i+1) ::                             acc
    | 0b0100 ->                         char (i+2) ::               acc
    | 0b0101 -> char i ::               char (i+2) ::               acc
    | 0b0110 ->           char (i+1) :: char (i+2) ::               acc
    | 0b0111 -> char i :: char (i+1) :: char (i+2) ::               acc
    | 0b1000 ->                                       char (i+3) :: acc
    | 0b1001 -> char i ::                             char (i+3) :: acc
    | 0b1010 ->           char (i+1) ::               char (i+3) :: acc
    | 0b1011 -> char i :: char (i+1) ::               char (i+3) :: acc
    | 0b1100 ->                         char (i+2) :: char (i+3) :: acc
    | 0b1101 -> char i ::               char (i+2) :: char (i+3) :: acc
    | 0b1110 ->           char (i+1) :: char (i+2) :: char (i+3) :: acc
    | _      -> char i :: char (i+1) :: char (i+2) :: char (i+3) :: acc
  in
  let nibs i db acc : char list =
    let b0 = (db lsr 0x0) land 0xf
    and b1 = (db lsr 0x4) land 0xf
    and b2 = (db lsr 0x8) land 0xf
    and b3 = (db lsr 0xc) land 0xf in
    match combine4 (b0 <> 0) (b1 <> 0) (b2 <> 0) (b3 <> 0) with
    | 0b0000 ->                                                         acc
    | 0b0001 -> bits i b0                                               acc
    | 0b0010 ->            bits (i+4) b1                                acc
    | 0b0011 -> bits i b0 (bits (i+4) b1                                acc)
    | 0b0100 ->                           bits (i+8) b2                 acc
    | 0b0101 -> bits i b0                (bits (i+8) b2                 acc)
    | 0b0110 ->            bits (i+4) b1 (bits (i+8) b2                 acc)
    | 0b0111 -> bits i b0 (bits (i+4) b1 (bits (i+8) b2                 acc))
    | 0b1000 ->                                          bits (i+12) b3 acc
    | 0b1001 -> bits i b0                               (bits (i+12) b3 acc)
    | 0b1010 ->            bits (i+4) b1                (bits (i+12) b3 acc)
    | 0b1011 -> bits i b0 (bits (i+4) b1                (bits (i+12) b3 acc))
    | 0b1100 ->                           bits (i+8) b2 (bits (i+12) b3 acc)
    | 0b1101 -> bits i b0                (bits (i+8) b2 (bits (i+12) b3 acc))
    | 0b1110 ->            bits (i+4) b1 (bits (i+8) b2 (bits (i+12) b3 acc))
    | _      -> bits i b0 (bits (i+4) b1 (bits (i+8) b2 (bits (i+12) b3 acc)))
  in
  let dbytes i dw acc : char list =
    let n0 = (dw lsr 0x00) land 0xffff
    and n1 = (dw lsr 0x10) land 0xffff
    and n2 = (dw lsr 0x20) land 0xffff
    and n3 = (dw lsr 0x30) land 0xffff in
    match combine4 (n0 <> 0) (n1 <> 0) (n2 <> 0) (n3 <> 0) with
    | 0b0000 ->                                                           acc
    | 0b0001 -> nibs i n0                                                 acc
    | 0b0010 ->            nibs (i+16) n1                                 acc
    | 0b0011 -> nibs i n0 (nibs (i+16) n1                                 acc)
    | 0b0100 ->                            nibs (i+32) n2                 acc
    | 0b0101 -> nibs i n0                 (nibs (i+32) n2                 acc)
    | 0b0110 ->            nibs (i+16) n1 (nibs (i+32) n2                 acc)
    | 0b0111 -> nibs i n0 (nibs (i+16) n1 (nibs (i+32) n2                 acc))
    | 0b1000 ->                                           nibs (i+48) n3  acc
    | 0b1001 -> nibs i n0                                 (nibs (i+48) n3 acc)
    | 0b1010 ->            nibs (i+16) n1                 (nibs (i+48) n3 acc)
    | 0b1011 -> nibs i n0 (nibs (i+16) n1                 (nibs (i+48) n3 acc))
    | 0b1100 ->                            nibs (i+32) n2 (nibs (i+48) n3 acc)
    | 0b1101 -> nibs i n0                 (nibs (i+32) n2 (nibs (i+48) n3 acc))
    | 0b1110 ->            nibs (i+16) n1 (nibs (i+32) n2 (nibs (i+48) n3 acc))
    | _      -> nibs i n0 (nibs (i+16) n1 (nibs (i+32) n2 (nibs (i+48) n3 acc)))
  in
    (dbytes 0 s.!(0)
       (dbytes 60 s.!(1)
          (dbytes 120 s.!(2)
             (dbytes 180 s.!(3)
                (dbytes 240 s.!(4) [])))))

let split (c : char) (s : t) =
  let v = s.!(0) and w = s.!(1) and x = s.!(2) and y = s.!(3) and z = s.!(4) in
  let i = int c in
  match c with
  | '\000'..'\059' -> [|v land ((1 lsl i) - 1); 0; 0; 0; 0|],
                      (v lsr i) land 1 <> 0,
                      [|v land (lnot ((1 lsl (i+1)) - 1)); w; x; y; z|]
  | '\060'..'\119' -> [|v; w land ((1 lsl (i-60)) - 1); 0; 0; 0|],
                      (w lsr (i-60)) land 1 <> 0,
                      [|0; w land (lnot ((1 lsl ((i-60)+1)) - 1)); x; y; z|]
  | '\120'..'\179' -> [|v; w; x land ((1 lsl (i-120)) - 1); 0; 0|],
                      (x lsr (i-120)) land 1 <> 0,
                      [|0; 0; x land (lnot ((1 lsl ((i-120)+1)) - 1)); y; z|]
  | '\180'..'\239' -> [|v; w; x; y land ((1 lsl (i-180)) - 1); 0|],
                      (y lsr (i-180)) land 1 <> 0,
                      [|0; 0; 0; y land (lnot ((1 lsl ((i-180)+1)) - 1)); z|]
  | _              -> [|v; w; x; y; z land ((1 lsl (i-240)) - 1)|],
                      (z lsr (i-240)) land 1 <> 0,
                      [|0; 0; 0; 0; z land (lnot ((1 lsl ((i-240)+1)) - 1))|]

let empty = fresh ()
let mem c s = get s c
let subset l r = is_empty (diff l r)

let map f s =
  let s' = fresh () in
  iter (fun c -> set s' (f c)) s;
  s'

let filter p s =
  let s' = fresh () in
  iter (fun c -> if p c then set s' c) s;
  s'

let partition p s =
  let syes = fresh () and sno = fresh () in
  iter (fun c -> set (if p c then syes else sno) c) s;
  (syes, sno)

let min_elt s =
  match first s with
  | -1 -> raise Not_found
  | c -> char c

let min_elt_opt s =
  match first s with
  | -1 -> None
  | c -> Some (char c)

let choose = min_elt
let choose_opt = min_elt_opt

let max_elt s =
  match last s with
  | -1 -> raise Not_found
  | c -> char c

let max_elt_opt s =
  match last s with
  | -1 -> None
  | c -> Some (char c)

let find c s =
  match mem c s with
  | false -> raise Not_found
  | true  -> c

let find_opt c s =
  match mem c s with
  | false -> None
  | true -> Some c

let find_first_opt p s =
  let rec loop p s = function
    | -1 -> None
    | c -> if p (char c) then Some (char c)
           else loop p s (next s (char c))
  in loop p s (first s)

let find_first p s =
  match find_first_opt p s with
  | None -> raise Not_found
  | Some c -> c

let find_last_opt p s =
  let rec loop s = function
    | -1 -> None
    | c -> if p (char c) then Some (char c)
           else loop s (prev s (char c))
  in loop s (last s)

let find_last p s =
  match find_last_opt p s with
  | None -> raise Not_found
  | Some c -> c

let filter_map p s =
  let s' = fresh () in
  iter (fun c -> match p c with None -> () | Some x -> set s' x) s;
  s'

let of_list elems =
  let s = fresh () in
  List.iter (set s) elems;
  s

let rec to_seq_from_option s = function
  | -1 -> Seq.Nil
  | c -> Seq.Cons (char c, fun () -> to_seq_from_option s (next s (char c)))

let rec to_rev_seq_from_option s = function
  | -1 -> Seq.Nil
  | c -> Seq.Cons (char c, fun () -> to_rev_seq_from_option s (prev s (char c)))

let to_seq_from c s = fun () -> to_seq_from_option s (if mem c s then int c else next s c)

let to_seq s = fun () -> to_seq_from_option s (first s)

let add_seq seq s =
  let s = clone s in
  Seq.iter (set s) seq;
  s

let of_seq seq =
  let s = fresh () in
  Seq.iter (set s) seq;
  s

let to_rev_seq s = fun () -> to_rev_seq_from_option s (last s)
