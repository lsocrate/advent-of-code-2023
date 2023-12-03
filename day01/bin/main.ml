open In_channel

let digits_of_line =
  let folder ds char =
    match int_of_string_opt @@ Char.escaped char with
    | None -> ds
    | Some n -> (
        match ds with
        | None, _ -> (Some n, Some n)
        | Some f, _ -> (Some f, Some n))
  in
  String.fold_left folder (None, None)

exception BadMathBro of string

let () =
  let sum_digits total line =
    match digits_of_line line with
    | Some first, Some second -> total + ((first * 10) + second)
    | _ -> raise (BadMathBro "the input is messed up")
  in
  open_in "./puzzle.input" |> input_lines
  |> List.fold_left sum_digits 0
  |> string_of_int |> print_endline
