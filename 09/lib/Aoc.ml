open! Core

let title = "AoC 2023 - Day 9"

(* PART 1 *)

let diff x =
  List.fold x ~init:(None, []) ~f:(fun (last, acc) x ->
    match last with
      | None -> (Some x, acc)
      | Some x' -> (Some x, (x - x') :: acc)
  ) |> snd |> List.rev

let predict x =
  if List.is_empty x then 0
  else let rec loop acc x' =
    if List.is_empty x' then acc
    else let x'' = diff x' in
      if List.for_all x'' ~f:(fun y -> y = 0) then acc
      else loop ((List.last_exn x'') :: acc) x''
  in loop [List.last_exn x] x
  |> List.fold ~init:0 ~f:(+) 

let read_oasis (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:[] ~f:(fun acc line ->
      let nums = String.split line ~on:' '
        |> List.map ~f:Int.of_string
      in nums :: acc
    ) |> List.rev
  )

let sum_histories o =
  List.fold o ~init:0 ~f:(fun acc x ->
    acc + (predict x)
  )

(* PART 2 *)

let bar (filename : string) =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun acc line ->
      acc + String.length(line)
    )
  )
