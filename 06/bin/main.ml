open! Core
open Aoc

let () =
  let rl = read_race "input.txt" in
  printf "%s\n" title;
  printf "Part 1 ways: %d\n" (me_count_the_ways rl);
  let r = read_race' "input.txt" in
  printf "Part 2 ways: %d\n" (ways_to_win r);
