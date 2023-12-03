open! Core
open Aoc

let () =
  let games = load_games "input.txt" in
  printf "%s\n" title;
  printf "Part 1 sum: %d\n" (sum_valid_games 
    games { red = 12; green = 13; blue = 14 });
  printf "Part 2 pow: %d\n" (pow_minima games)
