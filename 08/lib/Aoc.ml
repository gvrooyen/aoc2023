open! Core

let title = "AoC 2023 - Day 8"

(* PART 1 *)

type lr = Left | Right
  [@@deriving sexp, compare]

type fork = string * string

let fork_show f =
  "(" ^ fst f ^ ", " ^ snd f ^ ")"

type camelmap = fork Map.M(String).t

let camelmap_show cm =
  Map.fold cm ~init:"" ~f:(fun ~key ~data acc ->
    acc ^ (key ^ " -> " ^ (fork_show data) ^ "\n")
  )

type route = {
  turns: lr list;
  cm: camelmap;
}

let turns_of_string s =
  String.fold s ~init:[] ~f:(fun acc c ->
    match c with
      | 'L' -> Left :: acc
      | 'R' -> Right :: acc
      | _ -> failwith ("Unexpected turn character: " ^ (String.of_char c))
  ) |> List.rev

let readmap (filename : string) : route =
  let r = Str.regexp "\\(^[0-9A-Z]+\\) = (\\([0-9A-Z]+\\), \\([0-9A-Z]+\\))" in
  In_channel.with_file filename ~f:(fun file ->
    let turns = turns_of_string(In_channel.input_line_exn file) in
    let _ = In_channel.input_line_exn file in  (* skip blank line *)
    let cm = In_channel.fold_lines file ~init:(Map.empty (module String)) ~f:(fun acc line ->
      if Str.string_match r line 0 then
        let open Str in
        let key = matched_group 1 line in
        let data = (matched_group 2 line), (matched_group 3 line) in
        Map.set acc ~key ~data
      else failwith ("Syntax error: " ^ line)
    ) in
    {turns; cm}
  )

let count_steps (r : route) =
  let rec loop loc i n =
    if String.equal loc "ZZZ" then n
    else
      let i' = i mod (List.length r.turns) in
      let f = Map.find_exn r.cm loc in
      let next_loc = match List.nth_exn r.turns i' with
        | Left -> fst f
        | Right -> snd f
      in loop next_loc (i' + 1) (n + 1)
  in loop "AAA" 0 0

(* PART 2 *)

let check_last s c =
  let c' = String.get s (String.length s - 1) in
  Char.equal c' c

let ghost_steps (r : route) =
  let init = List.filter (Map.keys r.cm) ~f:(fun s -> check_last s 'A') in
  let rec loop locs i n =
    if List.for_all locs ~f:(fun s -> check_last s 'Z') then n
    else
      let i' = i mod (List.length r.turns) in
      let locs' = List.map locs ~f:(fun loc ->
        let f = Map.find_exn r.cm loc in
        match List.nth_exn r.turns i' with
          | Left -> fst f
          | Right -> snd f
      ) in
      loop locs' (i' + 1) (n + 1)
  in loop init 0 0
