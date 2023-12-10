module IntSet = Set.Make (Int)

let rec points_of_match_count = function
  | 0 -> 0
  | 1 -> 1
  | n -> 2 * points_of_match_count (n - 1)

let pick_string_parts line =
  match String.split_on_char ':' line with
  | [ _; numbers ] -> (
      match String.split_on_char '|' numbers with
      | [ winning; bought ] -> (String.trim winning, String.trim bought)
      | _ -> failwith @@ "Broken 2: " ^ numbers)
  | _ -> failwith @@ "Broken 1:" ^ line

let split_on_spaces = String.split_on_char ' '

let points_of_line line =
  let winning_str, bought_str = pick_string_parts line in
  let winning_numbers =
    split_on_spaces winning_str
    |> List.filter_map int_of_string_opt
    |> IntSet.of_list
  in
  let match_count =
    split_on_spaces bought_str
    |> List.filter_map (fun s ->
           match int_of_string_opt s with
           | Some n when IntSet.mem n winning_numbers -> Some n
           | _ -> None)
    |> List.length
  in
  if match_count < 1 then 0 else points_of_match_count match_count

let challenge1 input =
  let add_points t = function "" -> t | str -> t + points_of_line str in

  String.split_on_char '\n' input
  |> List.fold_left add_points 0
  |> string_of_int |> print_endline

let challenge2 _input = print_endline "not there yet"
let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
