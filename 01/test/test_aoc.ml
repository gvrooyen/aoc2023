open! Core
open Aoc

let%test_unit "scan_double_digits" =
  [%test_eq: int] (scan_num "ab1cde2fg3h") 13

let%test_unit "scan_single_digit" =
  [%test_eq: int] (scan_num "ab1cdefgh") 11

let%test_unit "calibration_1" =
  [%test_eq: int] (calibration_num "test.txt") 142

let%test_unit "scan_double_digits'" =
  [%test_eq: int] (scan_a' "ab1cde2fgnineh") 19

let%test_unit "scan_double_digits''" =
  [%test_eq: int] (scan_a' "two1nine") 29

let%test_unit "calibration_2" =
  [%test_eq: int] (calibration_a' "test2.txt") 281
