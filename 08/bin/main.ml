open! Core
open Aoc

let () =
  let r = readmap "input.txt" in
  printf "%s\n" title;
  printf "Part 1 steps: %d\n" (count_steps r);
  printf "Part 2 steps: %d\n" (ghost_steps r);
