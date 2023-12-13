open Utils

type race = { time : int; distance : int } [@@deriving show]

let numbers_of_line line =
  match String.split_on_char ':' line with
  | _ :: ns :: _ -> ints_of_line ns
  | _ -> failwith "nope"

let parts input =
  let to_race time distance = { time; distance } in
  match String.split_on_char '\n' input with
  | times_line :: distances_line :: _ ->
      List.map2 to_race
        (numbers_of_line times_line)
        (numbers_of_line distances_line)
  | _ -> failwith "nope"

let min_max_hold_time r =
  let goal_d = r.distance + 1 in
  (* -h2 + (r.time * h) - goal_d = 0 *)
  let a = -1. in
  let b = float_of_int r.time in
  let c = -1. *. float_of_int goal_d in
  let delta = sqrt (Float.pow b 2. -. (4. *. a *. c)) in
  let x1 = (-.b +. delta) /. (2. *. a) in
  let x2 = (-.b -. delta) /. (2. *. a) in
  let min, max = if x1 < x2 then (x1, x2) else (x2, x1) in
  (int_of_float @@ Float.ceil min, int_of_float @@ Float.floor max)

let ways_to_beat (min, max) = max - min + 1

let challenge1 input =
  parts input |> List.map min_max_hold_time
  |> List.fold_left (fun t rmm -> t * ways_to_beat rmm) 1
  |> string_of_int |> print_endline

let challenge2 _ = print_endline "not there yet"

let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
