open! Core
open! Aoc

let r = readmap "test.txt"

let%test_unit "readmap" =
  [%test_eq: rl list] r.turns [Right; Left]

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
