open! Core
open Aoc

let%test_unit "draw_of_string with all colors" =
  [%test_eq: draw] (draw_of_string " 3 green, 4 blue, 1 red")
    { red = 1; blue = 4; green = 3 }

let%test_unit "draw_of_string with some colors" =
  [%test_eq: draw] (draw_of_string "4 blue, 1 red")
    { red = 1; blue = 4; green = 0 }

let%test_unit "draw_of_string with no colors" =
  [%test_eq: draw] (draw_of_string "")
    { red = 0; blue = 0; green = 0 }

let%test_unit "game_of_string" =
  [%test_eq: game] (game_of_string "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    [ { red = 4; blue = 3; green = 0 };
      { red = 1; blue = 6; green = 2 };
      { red = 0; blue = 0; green = 2 };
    ]

let%expect_test _ =
  print_s [%sexp (load_games "test.txt" : game list)];
  [%expect {|
    ((((red 4) (green 0) (blue 3)) ((red 1) (green 2) (blue 6))
      ((red 0) (green 2) (blue 0)))
     (((red 0) (green 2) (blue 1)) ((red 1) (green 3) (blue 4))
      ((red 0) (green 1) (blue 1)))
     (((red 20) (green 8) (blue 6)) ((red 4) (green 13) (blue 5))
      ((red 1) (green 5) (blue 0)))
     (((red 3) (green 1) (blue 6)) ((red 6) (green 3) (blue 0))
      ((red 14) (green 3) (blue 15)))
     (((red 6) (green 3) (blue 1)) ((red 1) (green 2) (blue 2)))) |}]

let%test_unit "test_input_1" =
  let games = load_games "test.txt" in
  [%test_eq: int] (sum_valid_games games { red = 12; green = 13; blue = 14 }) 8

let%test_unit "test_input_2" =
  let games = load_games "test.txt" in
  [%test_eq: int] (pow_minima games) 2286
