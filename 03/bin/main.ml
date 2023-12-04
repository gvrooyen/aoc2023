open! Core
open Aoc

let () =
  let s = read_schematic "input.txt" in
  printf "%s\n" title;
  printf "Part 1 sum: %d\n" (sum_of_parts s);
  printf "Part 2 sum: %d\n" (sum_of_gears s);
