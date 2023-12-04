open In_channel

exception BadMathException of string

let merge old_pair new_d =
  match old_pair with
  | None, _ -> (Some new_d, Some new_d)
  | Some old_d1, _ -> (Some old_d1, Some new_d)

let rec parse_line old_pair str =
  if String.length str == 0 then
    match old_pair with
    | Some t, Some u -> (t * 10) + u
    | _ -> raise (BadMathException "Oops")
  else
    let new_pair str =
      if String.starts_with ~prefix:"one" str then merge old_pair 1
      else if String.starts_with ~prefix:"two" str then merge old_pair 2
      else if String.starts_with ~prefix:"three" str then merge old_pair 3
      else if String.starts_with ~prefix:"four" str then merge old_pair 4
      else if String.starts_with ~prefix:"five" str then merge old_pair 5
      else if String.starts_with ~prefix:"six" str then merge old_pair 6
      else if String.starts_with ~prefix:"seven" str then merge old_pair 7
      else if String.starts_with ~prefix:"eight" str then merge old_pair 8
      else if String.starts_with ~prefix:"nine" str then merge old_pair 9
      else
        match int_of_string_opt @@ String.sub str 0 1 with
        | Some n -> merge old_pair n
        | None -> old_pair
    in
    parse_line (new_pair str) (String.sub str 1 (String.length str - 1))

let () =
  open_in "./puzzle.input" |> input_lines
  |> List.map (parse_line (None, None))
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline
