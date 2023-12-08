open! Core

let title = "AoC 2023 - Day 7"

(* PART 1 *)

let cards = ['2'; '3'; '4'; '5'; '6'; '7';
  '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']

let card_rank c =
  cards |> List.findi ~f:(fun _ c' -> Char.equal c c')
  |> Option.value_exn |> fst

let card_compare c1 c2 =
  Int.compare (card_rank c1) (card_rank c2)

type hand_bid = string * int
  [@@deriving sexp, compare]

let read_handbids (filename : string) : hand_bid list =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
      match line |> String.split ~on:' ' with
        | [h; b] -> (h, Int.of_string b) :: acc
        | _ -> failwith "Syntax error"
    )
  )

type hand_type = K5 | K4 | FH | K3 | PP | P | H
  [@@deriving sexp, compare]

let enum_hand_type = function
  | K5 -> 6
  | K4 -> 5
  | FH -> 4
  | K3 -> 3
  | PP -> 2
  | P  -> 1
  | H  -> 0

type card_count = (char * int) list
  [@@deriving sexp, compare]

let count_cards s : card_count =
  if not (String.length s = 5) then failwith "invalid hand length"
  else
    String.fold s ~init:[] ~f:(fun acc c ->
      match List.Assoc.find acc ~equal:Char.equal c with
        | Some n -> List.Assoc.add acc ~equal:Char.equal c (n + 1)
        | None -> List.Assoc.add acc ~equal:Char.equal c 1
    )
    |> List.sort ~compare:(fun (_, n1) (_, n2) ->
          Int.compare n2 n1
    )

let hand_type_of_string s =
    match count_cards s with
      | [(_, 5)] -> K5
      |  (_, 4)  :: _ -> K4
      | [(_, 3); (_, 2)] -> FH
      |  (_, 3)  :: _ -> K3
      | [(_, 2); (_, 2); _] -> PP
      |  (_, 2) :: _ -> P
      | _ -> H

let hand_compare h1 h2 : int =
  let h1', h2' = (hand_type_of_string h1 |> enum_hand_type), 
    (hand_type_of_string h2 |> enum_hand_type) in
  if h1' < h2' then -1 else if h1' > h2' then 1 else
    List.zip_exn (String.to_list h1) (String.to_list h2)
    |> List.fold_until ~init:0 ~f:(fun acc (c1, c2) ->
        if Char.equal c1 c2 then Continue acc
        else Stop (card_compare c1 c2)
      ) ~finish:(fun acc -> acc)

let sort_hands (hb : hand_bid list) : hand_bid list =
  List.sort hb ~compare:(fun (h1, _) (h2, _) ->
    hand_compare h1 h2
  )

let winnings (hb : hand_bid list) : int =
  sort_hands hb
  |> List.foldi ~init:0 ~f:(fun i acc (_, b) ->
    acc + (i + 1) * b
  )

(* PART 2 *)

let bar (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + String.length(line)
    )
  )
