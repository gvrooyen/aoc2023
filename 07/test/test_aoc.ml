open! Core
open! Aoc

let%test_unit "card_rank" =
  [%test_eq: int] (card_rank '2') 0;
  [%test_eq: int] (card_rank '7') 5;
  [%test_eq: int] (card_rank 'A') 12

let%test_unit "card_compare" =
  [%test_eq: int] (card_compare '2' 'K') (-1);
  [%test_eq: int] (card_compare 'A' 'J') 1;
  [%test_eq: int] (card_compare 'T' 'T') 0

let%test_unit "card_count" =
  [%test_eq: card_count] (count_cards "AA3A3") [('A', 3); ('3', 2)]

let%test_unit "hand_type" =
  [%test_eq: hand_type] (hand_type_of_string "AAAAA") K5;
  [%test_eq: hand_type] (hand_type_of_string "A2AAA") K4;
  [%test_eq: hand_type] (hand_type_of_string "AA3A3") FH;
  [%test_eq: hand_type] (hand_type_of_string "A4AA5") K3;
  [%test_eq: hand_type] (hand_type_of_string "A6A62") PP;
  [%test_eq: hand_type] (hand_type_of_string "KK677") PP;
  [%test_eq: hand_type] (hand_type_of_string "KTJJT") PP;
  [%test_eq: hand_type] (hand_type_of_string "A78A9") P;
  [%test_eq: hand_type] (hand_type_of_string "23456") H

let%test_unit "hand_compare" =
  [%test_eq: int] (hand_compare "AAAAA" "A2AAA") 1;
  [%test_eq: int] (hand_compare "AA333" "AA333") 0;
  [%test_eq: int] (hand_compare "98765" "97JJ2") (-1);
  [%test_eq: int] (hand_compare "33332" "2AAAA") 1;
  [%test_eq: int] (hand_compare "77888" "77788") 1;
  [%test_eq: int] (hand_compare "KK677" "KTJJT") 1

let hb = read_handbids "test.txt"

let%expect_test _ =
  print_s [%sexp (sort_hands hb : hand_bid list)];
  [%expect {| ((32T3K 765) (KTJJT 220) (KK677 28) (T55J5 684) (QQQJA 483)) |}]

let%test_unit "winnings" =
  [%test_eq: int] (winnings hb) 6440
