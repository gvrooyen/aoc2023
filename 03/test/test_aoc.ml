open! Core
open Aoc

let%test_unit "pull_number Some x" =
  [%test_eq: int option] 
    (pull_number (Some "..123....") ~idx:3) (Some 123)

let%test_unit "pull_number None" =
  [%test_eq: int option]
    (pull_number (Some "..123....") ~idx:5) None

let%test_unit "pull_number neg idx" =
  [%test_eq: int option]
    (pull_number (Some "..123....") ~idx:(-1)) None

let%test_unit "pull_number oob idx" =
  [%test_eq: int option]
    (pull_number (Some "..123....") ~idx:(100)) None

let%test_unit "pull_number None string" =
  [%test_eq: int option]
    (pull_number None ~idx:0) None

let s = read_schematic "test.txt"

let%expect_test _ =
  let syms = find_symbols s in
  print_s [%sexp (syms : symbol list)];
  [%expect {|
    (((line 1) (col 3)) ((line 3) (col 6)) ((line 4) (col 3)) ((line 5) (col 5))
     ((line 8) (col 3)) ((line 8) (col 5))) |}]

let%expect_test _ =
  let partnums = find_partnums s 1 3 in
  print_s [%sexp (partnums : int list)];
  [%expect {| (35 467) |}]

let%expect_test _ =
  let parts = find_all_parts s in
  print_s [%sexp (parts : int list)];
  [%expect {| (35 467 633 617 592 664 598 755) |}]

let%test_unit "sum_of_parts" =
  [%test_eq: int] (sum_of_parts s) 4361

let%test_unit "sum_of_gears" =
  [%test_eq: int] (sum_of_gears s) 467835
