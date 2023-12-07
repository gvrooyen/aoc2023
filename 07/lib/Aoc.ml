open! Core

let title = "AoC 2023 - Day 7"

(* PART 1 *)

let cards = ['2'; '3'; '4'; '5'; '6'; '7';
  '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']

let card_rank c =
  cards |> List.findi ~f:(fun _ c' -> Char.equal c c')
  |> Option.value_exn |> fst

let foo (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + String.length(line)
    )
  )

type hand_type = K5 | K4 | FH | K3 | PP | P | Z
  [@@deriving sexp, compare]

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
      | _ -> Z

(* PART 2 *)

let bar (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + String.length(line)
    )
  )
