open! Core

let title = "AoC 2023 - Day 6"

(* PART 1 *)

type race = {
  t: int;
  d: int;
} [@@deriving sexp]

let nums_of_string s =
  String.split ~on:' ' s
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Int.of_string

let read_race (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    let s = In_channel.input_line_exn file in
    let times = match String.split ~on:':' s with
      | ["Time"; s'] -> nums_of_string s'
      | _ -> failwith "Invalid input"
    in
    let s = In_channel.input_line_exn file in
    let dists = match String.split ~on:':' s with
      | ["Distance"; s'] -> nums_of_string s'
      | _ -> failwith "Invalid input"
    in
    List.zip_exn times dists
    |> List.map ~f:(fun (t, d) -> {t; d})
  )

let ways_to_win (r : race) =
  let t = Float.of_int r.t in
  let d = Float.of_int r.d in
  let x = Float.sqrt(t*.t -. 4.*.d) in
  (* `eps` adds a small offset so that getting an _equal_ score doesn't
     equate to winning *)
  let eps = 0.0001 in
  let min = Float.round_up ((t -. x) /. 2. +. eps) in
  let max = Float.round_down ((t +. x) /. 2. -. eps) in
  Int.of_float (max -. min) + 1

let me_count_the_ways (rl : race list) =
  List.fold rl ~init:1 ~f:(fun acc r ->
    acc * ways_to_win r
  )

(* PART 2 *)

let read_race' (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    let s = In_channel.input_line_exn file in
    let t = match String.split ~on:':' s with
      | ["Time"; s'] -> 
          (String.filter s' ~f:(fun c -> Char.(c <> ' ')))
          |> Int.of_string
      | _ -> failwith "Invalid input"
    in
    let s = In_channel.input_line_exn file in
    let d = match String.split ~on:':' s with
      | ["Distance"; s'] ->
          (String.filter s' ~f:(fun c -> Char.(c <> ' ')))
          |> Int.of_string
      | _ -> failwith "Invalid input"
    in
    {t; d}
  )
