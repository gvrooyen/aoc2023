open! Core
open! Aoc

let%test_unit "card_rank" =
  [%test_eq: int] (card_rank '2') 0;
  [%test_eq: int] (card_rank '7') 5;
  [%test_eq: int] (card_rank 'A') 12

let%test_unit "card_count" =
  [%test_eq: card_count] (count_cards "AA3A3") [('A', 3); ('3', 2)]

let%test_unit "hand_type" =
  [%test_eq: hand_type] (hand_type_of_string "AAAAA") K5;
  [%test_eq: hand_type] (hand_type_of_string "A2AAA") K4;
  [%test_eq: hand_type] (hand_type_of_string "AA3A3") FH;
  [%test_eq: hand_type] (hand_type_of_string "A4AA5") K3;
  [%test_eq: hand_type] (hand_type_of_string "A6A62") PP;
  [%test_eq: hand_type] (hand_type_of_string "A78A9") P;
  [%test_eq: hand_type] (hand_type_of_string "23456") H

(* let%expect_test _ = *)
(*   print_s [%sexp (1 + 1 : int)]; *)
(*   [%expect {||}] *)
