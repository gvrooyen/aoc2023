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

(* Solution Approach
   =================
   For the current stage, calculate the location (`do_maps` through the remaining stages)
   for each range minimum and add it to the solution list. Then, determine which minima of
   the next stage fall within the ranges of the current stage, and repeat the process. This
   guarantees that all minima in the piecewise cascade of maps are tested so that the
   smallest solution can be found.
*)

type range = {
  min: int;
  max: int;
} [@@deriving sexp, compare]

let in_rangelist rl x =
  List.exists rl ~f:(fun r ->
    (x >= r.min) && (x <= r.max)
  )

let rangelist_of_seeds seeds =
  let rec loop rl seeds' =
    match seeds' with
      | [] -> List.rev rl
      | min :: len :: tl -> loop ({ min; max = min + len - 1} :: rl) tl
      | _ -> failwith "Seed list should contain an even number of integers"
  in loop [] seeds

let rangelist_of_maps (maps : pmap list) =
  List.fold maps ~init:[] ~f:(fun acc m ->
    { min = m.src; max = m.src + m.len - 1} :: acc
    ) |> List.sort ~compare:(fun r1 r2 ->
      Int.compare r1.min r2.min
    )

let find_closest_range (a : almanac) =
  let seedrange = rangelist_of_seeds a.seeds in
  let rec scan_minima acc rl maps =
    match maps with
      | [] -> acc
      | m :: tl ->
          let sl = 
  in
  scan_minima [] seedrange a.maps
  |> List.min_elt ~compare:Int.compare
