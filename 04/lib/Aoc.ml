open! Core

let title = "AoC 2023 - Day 4"

(* PART 1 *)

type card = {
  number: int;
  win: (int, Int.comparator_witness) Set.t;
  has: (int, Int.comparator_witness) Set.t;
}

let numset_of_string s =
  String.split ~on:' ' s
  |> List.filter_map ~f:Int.of_string_opt
  |> Set.of_list (module Int)

let card_of_string s =
  match String.split ~on:':' s with
    | [t; numstr] ->
        let n = match (String.split ~on:' ' t) 
          |> List.filter ~f:(fun s -> not (String.is_empty s)) with
            | [_; n'] -> Int.of_string n'
            | _ -> failwith "Invalid card title"
        in begin
        match String.split ~on:'|' numstr with
          | [win; has] -> {
              number = n;
              win = numset_of_string win;
              has = numset_of_string has;
            }
          | _ -> failwith "Too many lists for card"
        end
    | _ -> failwith "Only expected one ':' separator"

let score_card c =
  match (Set.length (Set.inter c.win c.has)) - 1 with
    | -1 -> 0
    | x -> Int.pow 2 x

let load_cards (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
      card_of_string line :: acc
    ) |> List.rev
  )

let total_score (filename : string) =
  load_cards filename
  |> List.fold ~init:0 ~f:(fun acc c -> acc + score_card c)

