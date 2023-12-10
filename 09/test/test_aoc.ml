open! Core
open! Aoc

let x = [0; 3; 6; 9; 12; 15]

let%test_unit "diff" =
  [%test_eq: int list] (diff x) [3; 3; 3; 3; 3]

let%test_unit "predict" =
  [%test_eq: int] (predict x) 18

let o = read_oasis "test.txt"

let%expect_test _ =
  print_s [%sexp (o : int list list)];
  [%expect {| ((0 3 6 9 12 15) (1 3 6 10 15 21) (10 13 16 21 30 45)) |}]

let%test_unit "sum_histories" =
  [%test_eq: int] (sum_histories o) 114

let x3 = [10; 13; 16; 21; 30; 45]

let%test_unit "predict_left" =
  [%test_eq: int] (predict_left x) (-3);
  [%test_eq: int] (predict_left x3) 5

let%test_unit "sum_histories_left" =
  [%test_eq: int] (sum_histories_left o) 2
