open! Core

let title = "AoC 2023 - Day ?"

(* PART 1 *)

let pull_number ~idx s =
  let c = String.get s idx in
  if not (Char.is_digit c) then None
  else
    let rec seek_left i w =
      if i < 0 then w
      else
        let c' = String.get s i in
        if Char.is_digit c' then seek_left (i - 1) ((Char.to_string c') ^ w)
        else w
    in
    let rec seek_right i w =
      if i >= String.length s then w
      else
        let c' = String.get s i in
        if Char.is_digit c' then seek_right (i + 1) (w ^ (Char.to_string c')) 
        else w
    in
    let w' = (seek_left (idx - 1) (Char.to_string c) |> seek_right (idx + 1)) in
    Some (int_of_string w')

let read_schematic (filename : string) =
  In_channel.with_file filename ~f:In_channel.input_line

(* PART 2 *)

let bar (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + String.length(line)
    )
  )
