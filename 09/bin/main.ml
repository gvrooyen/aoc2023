open! Core
open Aoc

let () =
  let o = read_oasis "input.txt" in
  printf "%s\n" title;
  printf "Part 1 sum: %d\n" (sum_histories o);
  printf "Part 2 sum: %d\n" (sum_histories_left o);
