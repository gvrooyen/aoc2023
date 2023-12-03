open! Core
open Aoc

let%test_unit "pull_number Some x" =
  [%test_eq: int option] 
    (pull_number "..123...." ~idx:3) (Some 123)

let%test_unit "pull_number None" =
  [%test_eq: int option]
    (pull_number "..123...." ~idx:5) None

let%expect_test _ =
  print_s [%sexp (1 + 1 : int)];
  [%expect {| 2 |}]
