(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2

module Seq =
struct
  (* for pre-4.14 compatibility *)
  let[@ocaml.warning "-32"] rec equal eq (l : _ Seq.t) (r : _ Seq.t) =
    match l (), r () with
    | Nil, Nil -> true
    | Nil, _ | _, Nil -> false
    | Cons (x, xs), Cons (y, ys) -> eq x y && equal eq xs ys
  include Stdlib.Seq
end

let result_of_exn f = match f () with v -> Ok v | exception e -> Error e


let print_elements : 'a. ('a -> string) -> 'a list -> string =
  let rec loop f = function
    | [] -> "}"
    | [b] -> Printf.sprintf "%s}" (f b)
    | b :: bs -> Printf.sprintf "%s, %s" (f b) (loop f bs)
  in fun f bs -> "{"^ loop f bs


let print_bools = print_elements (Printf.sprintf "%b")


let print_bool_result = function
  | Ok b -> Printf.sprintf "Ok %b" b
  | Error e -> Printf.sprintf "Error %s" (Printexc.to_string e)


module type charset = module type of Charset
module C1 : charset = Charset
module C2 : charset =
struct
  module C = Set.Make(Char)
  (* inefficient version for pre-4.12 compatibility *)
  let[@ocaml.warning "-32"] to_rev_seq s = List.to_seq (List.rev (C.elements s))
  let[@ocaml.warning "-32"] to_list = C.elements
  include C
end

let print_chars = print_elements (Printf.sprintf "%c")

let assert_eq_charsets l r =
  assert_equal ~printer:print_chars (C1.elements l) (C2.elements r)

let assert_eq_seq l r =
  assert_equal ~cmp:(Seq.equal Char.equal) l r

let chars = List.init 256 Char.chr

let char_sets = Testsets.char_sets

let char_predicates = [
    ((=) '\255'); 
    ((=) '\000'); 
    ((>=) 'Q'); 
    ((<) 'Q');
    (fun c -> c < 'a' || c > '\240');
    (fun c -> c = '\004' || c = '\006');
    (fun c -> 'a' <= c && c <= 'z');
    (fun c -> '0' <= c && c <= '9');
    (fun _ -> false);
    (fun _ -> true);
  ]


let char_functions = [
    Char.lowercase_ascii; 
    Char.uppercase_ascii;
    (fun c -> Char.chr ((Char.code c * 37) mod 255));
  ]

let monotonic_char_functions =
  List.init 257 (fun i c -> i <= Char.code c)

let monotonic_decreasing_char_functions =
  List.init 257 (fun i c -> i >= Char.code c)

let for_all_charsets : (C1.t -> C2.t -> unit) -> unit =
  let c1s = List.map C1.of_list char_sets
  and c2s = List.map C2.of_list char_sets in
  fun f -> List.iter2 f c1s c2s


let for_all_chars : (char -> unit) -> unit =
  fun k -> List.iter k chars

let for_all_char_predicates : ((char -> bool) -> unit) -> unit =
  fun k -> List.iter k char_predicates

let for_all_char_functions : ((char -> char) -> unit) -> unit =
  fun k -> List.iter k char_functions

let for_all_monotonic_char_functions : ((char -> bool) -> unit) -> unit =
  fun k -> List.iter k monotonic_char_functions

let for_all_monotonic_decreasing_char_functions : ((char -> bool) -> unit) -> unit =
  fun k -> List.iter k monotonic_decreasing_char_functions


let intrinsics_tests _ =
  let open Charset_bitops in
  begin
    assert_equal 28 (clz 0x400000000);
    assert_equal 34 (ctz 0x400000000);
    assert_equal 24 (popcnt 0xdeadbeef);
  end


let empty_tests _ =
  (* val empty : t *)
  assert_eq_charsets C1.empty C2.empty
    

let is_empty_tests _ =
  (* val is_empty : t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal ~printer:string_of_bool
    (C1.is_empty c1s)
    (C2.is_empty c2s)
  end


let mem_tests _ =
  (* val mem : elt -> t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_chars @@ fun c ->
  assert_equal ~printer:string_of_bool
    (C1.mem c c1s)
    (C2.mem c c2s)
  end
  

let add_tests _ =
  (* val add : elt -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_chars @@ fun c ->
  assert_eq_charsets
    (C1.add c c1s)
    (C2.add c c2s)
  end


let singleton_tests _ =
  (* val singleton : elt -> t *)
  begin
  for_all_chars @@ fun c ->
  assert_eq_charsets
    (C1.singleton c)
    (C2.singleton c)
  end


let remove_tests _ =
  (* val remove : elt -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_chars @@ fun c ->
  assert_eq_charsets
    (C1.remove c c1s)
    (C2.remove c c2s)
  end


let union_tests _ =
  (* val union : t -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_eq_charsets
    (C1.union c1s c1s')
    (C2.union c2s c2s')
  end


let inter_tests _ =
  (* val inter : t -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_eq_charsets
    (C1.inter c1s c1s')
    (C2.inter c2s c2s')
  end


let disjoint_tests _ =
  (* val disjoint : t -> t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_equal ~printer:string_of_bool
    (C1.disjoint c1s c1s')
    (C2.disjoint c2s c2s')
  end


let diff_tests _ =
  (* val diff : t -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_eq_charsets
    (C1.diff c1s c1s')
    (C2.diff c2s c2s')
  end


let compare_tests _ =
  (* val compare : t -> t -> int *)
  (* Not tested, since ordering is arbitrary: it's consistent, but not necessarily
     the same as the standard library ordering. *)
  skip_if true
    "Not tested, since ordering is arbitrary";
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_equal ~printer:string_of_int
    (C1.compare c1s c1s')
    (C2.compare c2s c2s')
  end


let equal_tests _ =
  (* val equal : t -> t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_equal ~printer:string_of_bool
    (C1.equal c1s c1s')
    (C2.equal c2s c2s')
  end


let subset_tests _ =
  (* val subset : t -> t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c1s' c2s' ->
  assert_equal ~printer:string_of_bool
    (C1.subset c1s c1s')
    (C2.subset c2s c2s')
  end


let iter_tests _ =
  (* val iter : (elt -> unit) -> t -> unit *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  let r1 = ref [] in
  let r2 = ref [] in
  C1.iter (fun c -> r1 := c :: !r1) c1s;
  C2.iter (fun c -> r2 := c :: !r2) c2s;
  assert_equal !r1 !r2
  end


let map_tests _ =
  (* val map : (elt -> elt) -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_char_functions @@ fun f ->
  assert_eq_charsets
    (C1.map f c1s)
    (C2.map f c2s)
  end


let fold_tests _ =
  (* val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal ~printer:print_chars
    (C1.fold List.cons c1s [])
    (C2.fold List.cons c2s [])
  end


let for_all_tests _ =
  (* val for_all : (elt -> bool) -> t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_char_predicates @@ fun p ->
  assert_equal ~printer:string_of_bool
    (C1.for_all p c1s)
    (C2.for_all p c2s)
  end


let exists_tests _ =
  (* val exists : (elt -> bool) -> t -> bool *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_char_predicates @@ fun p ->
  assert_equal ~printer:string_of_bool
    (C1.exists p c1s)
    (C2.exists p c2s)
  end


let filter_tests _ =
  (* val filter : (elt -> bool) -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_char_predicates @@ fun p ->
  assert_eq_charsets
    (C1.filter p c1s)
    (C2.filter p c2s)
  end


let filter_map_tests _ =
  (* val filter_map : (elt -> elt option) -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_char_predicates @@ fun p ->
  for_all_char_functions @@ fun g ->
  let f c = if p c then Some (g c) else None in
  assert_eq_charsets
    (C1.filter_map f c1s)
    (C2.filter_map f c2s)
  end


let partition_tests _ =
  (* val partition : (elt -> bool) -> t -> t * t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_char_predicates @@ fun p ->
  let p1, p1' = C1.partition p c1s in
  let p2, p2' = C2.partition p c2s in
  assert_eq_charsets p1  p2 ;
  assert_eq_charsets p1' p2';
  end


let cardinal_tests _ =
  (* val cardinal : t -> int *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal ~printer:string_of_int
    (C1.cardinal c1s)
    (C2.cardinal c2s)
  end


let elements_tests _ =
  (* val elements : t -> elt list *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (C1.elements c1s)
    (C2.elements c2s)
  end


let min_elt_tests _ =
  (* val min_elt : t -> elt *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (result_of_exn @@ fun () -> C1.min_elt c1s)
    (result_of_exn @@ fun () -> C2.min_elt c2s)
  end


let min_elt_opt_tests _ =
  (* val min_elt_opt : t -> elt option *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (C1.min_elt_opt c1s)
    (C2.min_elt_opt c2s)
  end


let max_elt_tests _ =
  (* val max_elt : t -> elt *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (result_of_exn @@ fun () -> C1.max_elt c1s)
    (result_of_exn @@ fun () -> C2.max_elt c2s)
  end


let max_elt_opt_tests _ =
  (* val max_elt_opt : t -> elt option *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (C1.max_elt_opt c1s)
    (C2.max_elt_opt c2s)
  end


let choose_tests _ =
  (* val choose : t -> elt *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (result_of_exn @@ fun () -> C1.choose c1s)
    (result_of_exn @@ fun () -> C2.choose c2s)
  end


let choose_opt_tests _ =
  (* val choose_opt : t -> elt option *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_equal
    (C1.choose_opt c1s)
    (C2.choose_opt c2s)
  end


let split_tests _ =
  (* val split : elt -> t -> t * bool * t *)
  begin
    for_all_charsets @@ fun c1s c2s ->
    for_all_chars @@ fun c ->
        let a,b,c = C1.split c c1s
        and d,e,f = C2.split c c2s in
        assert_eq_charsets a d;
        assert_equal b e;
        assert_eq_charsets c f;
  end


let find_tests _ =
  (* val find : elt -> t -> elt *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_chars @@ fun c ->
  assert_equal
    (result_of_exn @@ fun () -> C1.find c c1s)
    (result_of_exn @@ fun () -> C2.find c c2s)
  end


let find_opt_tests _ =
  (* val find_opt : elt -> t -> elt option *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_chars @@ fun c ->
  assert_equal
    (C1.find_opt c c1s)
    (C2.find_opt c c2s)
  end


let find_first_tests _ =
  (* val find_first : (elt -> bool) -> t -> elt *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_monotonic_char_functions @@ fun f ->
  assert_equal
    (result_of_exn @@ fun () -> C1.find_first f c1s)
    (result_of_exn @@ fun () -> C2.find_first f c2s)
  end


let find_first_opt_tests _ =
  (* val find_first_opt : (elt -> bool) -> t -> elt option *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_monotonic_char_functions @@ fun f ->
  assert_equal
    (C1.find_first_opt f c1s)
    (C2.find_first_opt f c2s)
  end


let find_last_tests _ =
  (* val find_last : (elt -> bool) -> t -> elt *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_monotonic_decreasing_char_functions @@ fun f ->
  assert_equal
    (result_of_exn @@ fun () -> C1.find_last f c1s)
    (result_of_exn @@ fun () -> C2.find_last f c2s)
  end


let find_last_opt_tests _ =
  (* val find_last_opt : (elt -> bool) -> t -> elt option *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_monotonic_decreasing_char_functions @@ fun f ->
  assert_equal
    (C1.find_last_opt f c1s)
    (C2.find_last_opt f c2s)
  end


let of_list_tests _ =
  (* val of_list : elt list -> t *)
  begin
  for_all_charsets @@ fun c1s _ ->
  let l = C1.elements c1s in
  assert_eq_charsets
    (C1.of_list l)
    (C2.of_list l)
  end


let to_seq_from_tests _ =
  (* val to_seq_from : elt -> t -> elt Seq.t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_chars @@ fun c ->
  assert_equal ~printer:print_chars
    (List.of_seq (C1.to_seq_from c c1s))
    (List.of_seq (C2.to_seq_from c c2s))
  end


let to_seq_tests _ =
  (* val to_seq : t -> elt Seq.t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_eq_seq
    (C1.to_seq c1s)
    (C2.to_seq c2s)
  end


let to_rev_seq_tests _ =
  (* val to_rev_seq : t -> elt Seq.t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_eq_seq
    (C1.to_rev_seq c1s)
    (C2.to_rev_seq c2s)
  end


let add_seq_tests _ =
  (* val add_seq : elt Seq.t -> t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  for_all_charsets @@ fun c3s c4s ->
  assert_eq_charsets
    (C1.add_seq (C1.to_seq c3s) c1s)
    (C2.add_seq (C2.to_seq c4s) c2s)
  end


let of_seq_tests _ =
  (* val of_seq : elt Seq.t -> t *)
  begin
  for_all_charsets @@ fun c1s c2s ->
  assert_eq_charsets
    (C1.of_seq (C1.to_seq c1s))
    (C2.of_seq (C2.to_seq c2s))
  end


let suite = "Set tests" >::: [
      "intrinsics"     >:: intrinsics_tests;

      "empty"          >:: empty_tests;
      "is_empty"       >:: is_empty_tests;
      "mem"            >:: mem_tests;
      "add"            >:: add_tests;
      "singleton"      >:: singleton_tests;
      "remove"         >:: remove_tests;
      "union"          >:: union_tests;
      "inter"          >:: inter_tests;
      "disjoint"       >:: disjoint_tests;
      "diff"           >:: diff_tests;
      "compare"        >:: compare_tests;
      "equal"          >:: equal_tests;
      "subset"         >:: subset_tests;
      "iter"           >:: iter_tests;
      "map"            >:: map_tests;
      "fold"           >:: fold_tests;
      "for_all"        >:: for_all_tests;
      "exists"         >:: exists_tests;
      "filter"         >:: filter_tests;
      "filter_map"     >:: filter_map_tests;
      "partition"      >:: partition_tests;
      "cardinal"       >:: cardinal_tests;
      "elements"       >:: elements_tests;
      "min_elt"        >:: min_elt_tests;
      "min_elt_opt"    >:: min_elt_opt_tests;
      "max_elt"        >:: max_elt_tests;
      "max_elt_opt"    >:: max_elt_opt_tests;
      "choose"         >:: choose_tests;
      "choose_opt"     >:: choose_opt_tests;
      "split"          >:: split_tests;
      "find"           >:: find_tests;
      "find_opt"       >:: find_opt_tests;
      "find_first"     >:: find_first_tests;
      "find_first_opt" >:: find_first_opt_tests;
      "find_last"      >:: find_last_tests;
      "find_last_opt"  >:: find_last_opt_tests;
      "of_list"        >:: of_list_tests;
      "to_seq_from"    >:: to_seq_from_tests;
      "to_seq"         >:: to_seq_tests;
      "to_rev_seq"     >:: to_rev_seq_tests;
      "add_seq"        >:: add_seq_tests;
      "of_seq"         >:: of_seq_tests;
    ]


let _ =
  run_test_tt_main suite
