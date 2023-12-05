open! Core
open Aoc

let () =
  let a = read_almanac "input.txt" in
  printf "%s\n" title;
  printf "Part 1 closest: %d\n" (find_closest a);
