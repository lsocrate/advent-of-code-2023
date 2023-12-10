module IntSet = Set.Make (Int)

type card = { won : int }

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

let card_of_line line =
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
  { won = (if match_count < 1 then 0 else match_count) }

let challenge1 input =
  let add_points t = function
    | "" -> t
    | str -> t + points_of_match_count (card_of_line str).won
  in
  String.split_on_char '\n' input
  |> List.fold_left add_points 0
  |> string_of_int |> print_endline

let rec process idx cgs =
  let update_cgs c =
    for n = idx + 1 to idx + c.won do
      let old_list = Array.get cgs n in
      let new_list = List.hd old_list :: old_list in
      Array.set cgs n new_list
    done
  in
  if idx + 1 == Array.length cgs then cgs
  else (
    List.iter update_cgs (Array.get cgs idx);
    process (idx + 1) cgs)

let challenge2 input =
  let count_cards t (cgs : card list) = t + List.length cgs in
  String.split_on_char '\n' input
  |> List.filter_map (fun line ->
         if String.length line == 0 then None else Some [ card_of_line line ])
  |> Array.of_list |> process 0
  |> Array.fold_left count_cards 0
  |> string_of_int |> print_endline

let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
