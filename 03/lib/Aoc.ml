open! Core

let title = "AoC 2023 - Day 3"

(* PART 1 *)

type schematic = string list

type symbol = {
  line: int;
  col: int;
} [@@deriving sexp]

let pull_number ~idx s' =
  if (Option.is_none s') || (idx < 0) then None
  else 
    let s = Option.value_exn s' in
    if idx >= (String.length s) then None
    else
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
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
      line :: acc
    ) |> List.rev
  )

let find_symbols (s : schematic) =
  List.foldi s ~init:[] ~f:(fun i acc line ->
    String.foldi line ~init:acc ~f:(fun j acc c ->
      match c with
        | '.' -> acc
        | c' when Char.is_digit c' -> acc
        | _ -> { line = i; col = j } :: acc
    )
  ) |> List.rev

let find_partnums s line col =
  let dirs = [(-1, -1); (-1, 0); (-1, 1);
    (0, -1); (0, 1);
    (1, -1); (1, 0); (1, 1)] in
  List.map dirs ~f:(fun (dy, dx) ->
    pull_number (List.nth s (line + dy)) ~idx:(col + dx)
  ) |> List.filter_map ~f:Fn.id
    |> List.dedup_and_sort ~compare:Int.compare

let find_all_parts s =
  let syms = find_symbols s in
  List.fold syms ~init:[] ~f:(fun acc sym ->
    acc @ (find_partnums s sym.line sym.col)
  ) 

let sum_of_parts s =
  List.sum (module Int) (find_all_parts s) ~f:Fn.id

(* PART 2 *)

let find_ast (s : schematic) =
  List.foldi s ~init:[] ~f:(fun i acc line ->
    String.foldi line ~init:acc ~f:(fun j acc c ->
      match c with
        | '*' -> { line = i; col = j } :: acc
        | _ ->  acc
    )
  ) |> List.rev

let find_gears s =
  let ast = find_ast s in
  List.fold ast ~init:[] ~f:(fun acc pos ->
    let gearnums = find_partnums s pos.line pos.col in
    if List.length gearnums = 2 then
      (List.nth_exn gearnums 0) * (List.nth_exn gearnums 1) :: acc
    else acc
  )

let sum_of_gears s =
  List.sum (module Int) (find_gears s) ~f:Fn.id
