open! Core
open! Aoc

let card = card_of_string "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"

let%test_unit "card_of_string" =
  [%test_eq: int] (card.number) 3;
  [%test_eq: bool] (Set.exists (card.win) ~f:(fun x -> x = 53)) true;
  [%test_eq: bool] (Set.exists (card.win) ~f:(fun x -> x = 69)) false;
  [%test_eq: bool] (Set.exists (card.has) ~f:(fun x -> x = 82)) true;
  [%test_eq: bool] (Set.exists (card.has) ~f:(fun x -> x = 53)) false
  
let%test_unit "score_card" =
  [%test_eq: int] (score_card card) 2

let card = card_of_string (
  "Card   4: 83 15  5  4  3 92 86  2 91 58 " ^
  "| 58  2 77 32 93 64 37 82 47 28 13 89 16 24 59 76 52 50 55 15 14 68 61 91 98"
)

let%test_unit "card_of_string long" =
  [%test_eq: int] (card.number) 4;
  [%test_eq: bool] (Set.exists (card.win) ~f:(fun x -> x = 15)) true

let%test_unit "total_score" =
  [%test_eq: int] (total_score "test.txt") 13

