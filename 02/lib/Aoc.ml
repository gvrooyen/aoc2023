open! Core

let title = "AoC 2023 - Day 2"

(* PART 1 *)

type draw = {
  red: int;
  green: int;
  blue: int;
} [@@deriving sexp, compare]

let draw_of_string s = 
  String.split ~on:',' s
  |> List.fold ~init:({red = 0; green = 0; blue = 0}) ~f:(fun acc w ->
    let w' = String.strip w in
    match String.split ~on:' ' w' with
      | [] | [_] -> acc
      | [x; "red"] -> {acc with red = (int_of_string x)}
      | [x; "green"] -> {acc with green = (int_of_string x)}
      | [x; "blue"] -> {acc with blue = (int_of_string x)}
      | _ -> failwith "Unexpected color or draw syntax"
  )

type game = draw list
  [@@deriving sexp, compare]

let game_of_string s =
  match String.split ~on:':' s with
    | [_; draws] ->
        String.split ~on:';' draws
        |> List.map ~f:draw_of_string
    | _ -> failwith "Only expected one ':' separator"

let load_games (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
      (game_of_string line) :: acc
    ) |> List.rev
  )

let sum_valid_games (games : game list) (maxima : draw) =
  List.foldi games ~init:0 ~f:(fun i acc game ->
    if List.for_all game ~f:(fun g ->
      g.red <= maxima.red &&
      g.green <= maxima.green &&
      g.blue <= maxima.blue
    ) then acc + i + 1
    else acc
  )

(* PART 2 *)

let pow_minima (games : game list) =
  List.fold games ~init:0 ~f:(fun acc game ->
    let m = List.fold game 
      ~init:{ red = 0; green = 0; blue = 0 }
      ~f:(fun acc' d ->
        { red = Int.max acc'.red d.red;
          green = Int.max acc'.green d.green;
          blue = Int.max acc'.blue d.blue;
        }
    ) in
    acc + (m.red * m.green * m.blue)
  )
