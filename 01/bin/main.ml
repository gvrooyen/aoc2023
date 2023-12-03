open! Core
open Aoc

let () =
  let x = calibration_num "input.txt" in
  printf "%s\n" title;
  printf "Part 1 calibration: %d\n" x;
  let x = calibration_a' "input.txt" in
  printf "Part 2 calibration: %d\n" x
