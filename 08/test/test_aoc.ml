open! Core
open! Aoc

let r = readmap "test.txt"

let%test_unit "readmap" =
  [%test_eq: lr list] r.turns [Right; Left]

let%expect_test _ =
  print_endline (camelmap_show r.cm);
  [%expect {|
    AAA -> (BBB, CCC)
    BBB -> (DDD, EEE)
    CCC -> (ZZZ, GGG)
    DDD -> (DDD, DDD)
    EEE -> (EEE, EEE)
    GGG -> (GGG, GGG)
    ZZZ -> (ZZZ, ZZZ) |}]

let%test_unit "count_steps" =
  [%test_eq: int] (count_steps r) 2

let r = readmap "test2.txt"

let%test_unit "ghost_steps" =
  [%test_eq: int] (ghost_steps r) 6
