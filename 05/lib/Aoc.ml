open! Core

let title = "AoC 2023 - Day 5"

(* PART 1 *)

type pmap = {
  src: int;
  dst: int;
  len: int;
} [@@deriving sexp]

type almanac = {
  seeds: int list;
  maps: (pmap list) list;
} [@@deriving sexp]

let do_map (m : pmap) x =
  if (x >= m.src) && (x < m.src + m.len) then
    Some (x - m.src + m.dst)
  else None

let do_maps (maps : pmap list) x =
  List.fold_until maps ~init:x
    ~f:(fun acc m ->
      match do_map m x with
        | Some x' -> Stop x'
        | None -> Continue acc
    ) ~finish:Fn.id

let nums_of_string s =
  String.split ~on:' ' s
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Int.of_string

let read_almanac (filename : string) : almanac =
  In_channel.with_file filename ~f:(fun file ->
    match String.split ~on:':' (In_channel.input_line_exn file) with
      | ["seeds"; s] ->
          let seeds = nums_of_string s in
          let _ = In_channel.input_line_exn file in (* discard blank line *)
          let _, acc', maps' = In_channel.fold_lines file ~init:(true, [], []) ~f:(fun (hdr, acc, maps) line ->
            if hdr then
              (* discard header *)
              (false, acc, [])
            else
              if String.is_empty line then
                (* discard blank line *)
                (true, ((maps |> List.rev) :: acc), [])
              else
                let nums = nums_of_string line in
                if (List.length nums) <> 3 then
                  failwith "Too many numbers in line"
                else
                  let m = { src = List.nth_exn nums 1;
                    dst = List.nth_exn nums 0;
                    len = List.nth_exn nums 2;
                  } in
                  (false, acc, m :: maps)
          ) in
          { seeds; maps = List.rev ((maps' |> List.rev) :: acc') }
      | _ -> failwith "Syntax error reading seed list"
  )

let find_closest (a : almanac) =
  List.map a.seeds ~f:(fun x ->
    List.fold a.maps ~init:x ~f:(fun acc m -> do_maps m acc)
  )
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

(* PART 2 *)

type range = {
  min: int;
  max: int;
} [@@deriving sexp, compare]

(** Take the first range list, and split it in places where there are discontinuities
    in the second range list. The result will be the same number of ranges or more as
    in the first list, and will cover all integers contained in the first list. Ranges
    where `r2` don't overlap with `r1` will fall away; ranges where `r1` doesn't overlap
    with `r2` will "fall through". We assume both lists are sorted by `min`, and that
    there is no self-overlap in the individual range lists. *)
let intersect (r1 : range list) (r2 : range list) : range list =
  let rec loop acc r1' r2' =
    match r1', r2' with
      | [], _ -> List.rev acc
      | _, [] -> (List.rev acc) @ r1'
      | hd1 :: tl1, hd2 :: tl2 ->
          if hd1.min < hd2.min then
            if hd1.max < hd2.min then
              loop (hd1 :: acc) tl1 r2'
            else
              loop ({min = hd1.min; max = hd2.min - 1} :: acc) ({min = hd2.min; max = hd1.max} :: tl1) r2'
          else
            
  in loop [] r1 r2

(* let find_closest_range (a : almanac) = *)
(*   _ *)
