open! Core
open! Aoc

let%test_unit "ways_to_win" =
  [%test_eq: int] (ways_to_win { t = 7; d = 9 } ) 4;
  [%test_eq: int] (ways_to_win { t = 15; d = 40 } ) 8;
  [%test_eq: int] (ways_to_win { t = 30; d = 200 } ) 9

let rl = read_race "test.txt"

let%expect_test _ =
  print_s [%sexp (rl : race list)];
  [%expect {| (((t 7) (d 9)) ((t 15) (d 40)) ((t 30) (d 200))) |}]

let%test_unit "me_count_the_ways" =
  [%test_eq: int] (me_count_the_ways rl) 288
