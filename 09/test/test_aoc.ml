open! Core
open! Aoc

let%test_unit "test_name" =
  [%test_eq: int] (1 + 1) 2

let%expect_test _ =
  print_s [%sexp (1 + 1 : int)];
  [%expect {||}]
