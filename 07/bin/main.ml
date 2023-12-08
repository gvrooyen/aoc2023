open! Core
open Aoc

let () =
  let hb = read_handbids "input.txt" in
  printf "%s\n" title;
  printf "Part 1 winnings: %d\n" (winnings hb);
