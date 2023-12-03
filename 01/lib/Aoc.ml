open! Core

let title = "AoC 2023 - Day 1"

(* PART 1 *)

let push acc x =
  match acc with
    | (None, _) -> (Some x, None)
    | (a, _) -> (a, Some x)

let base10 a b =
  match a, b with
    | (Some a, Some b) -> a * 10 + b
    | (Some a, None) -> a * 10 + a
    | _ -> failwith "String does not contain any digits"

let scan_num s =
  let a, b = String.fold ~init:(None, None) s ~f:(fun acc c ->
    if Char.is_digit c then push acc (Char.to_int c - 48)
    else acc
  ) in base10 a b

let calibration_num (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + scan_num line
  ))

(* PART 2 *)

let nums = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let scan_a' s =
  let a, b = String.foldi ~init:(None, None) s ~f:(fun i acc c ->
    if Char.is_digit c then
      push acc (Char.to_int c - 48)
    else
      match List.findi nums ~f:(fun _ num ->
        let open String in
        let sofar = slice s (Int.max 0 (i - 5)) (i + 1) in
        equal (suffix sofar (length num)) num
      ) with
        | Some (j, _) -> push acc (j + 1)
        | None -> acc
  ) in base10 a b

let calibration_a' (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + scan_a' line
  ))
