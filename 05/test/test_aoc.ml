open! Core
open! Aoc

let%test_unit "do_map in range" =
  let m = { src = 98; dst = 50; len = 2; } in
  [%test_eq: int option] (do_map m 99) (Some 51);
  [%test_eq: int option] (do_map m 97) None

let%test_unit "nums_of_string" =
  [%test_eq: int list] (nums_of_string "1 2 3") [1; 2; 3]

let%expect_test _ =
  print_s [%sexp ( read_almanac "test.txt" : almanac )];
  [%expect {|
    ((seeds (79 14 55 13))
     (maps
      ((((src 98) (dst 50) (len 2)) ((src 50) (dst 52) (len 48)))
       (((src 15) (dst 0) (len 37)) ((src 52) (dst 37) (len 2))
        ((src 0) (dst 39) (len 15)))
       (((src 53) (dst 49) (len 8)) ((src 11) (dst 0) (len 42))
        ((src 0) (dst 42) (len 7)) ((src 7) (dst 57) (len 4)))
       (((src 18) (dst 88) (len 7)) ((src 25) (dst 18) (len 70)))
       (((src 77) (dst 45) (len 23)) ((src 45) (dst 81) (len 19))
        ((src 64) (dst 68) (len 13)))
       (((src 69) (dst 0) (len 1)) ((src 0) (dst 1) (len 69)))
       (((src 56) (dst 60) (len 37)) ((src 93) (dst 56) (len 4)))))) |}]
