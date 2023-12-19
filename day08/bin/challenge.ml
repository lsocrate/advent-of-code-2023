type t = { easy : string list -> unit; hard : string  list -> unit }

let run_challenge (c:t) =
  let input =
    match Sys.argv.(1) with
    | "test" -> "./test.input"
    | "puzzle" -> "./puzzle.input"
    | _ -> failwith "Invalid input file"
  in
  let target_challenge =
    match Sys.argv.(2) with
    | "easy" -> c.easy
    | "hard" -> c.hard
    | _ -> failwith "Choose level"
  in
  open_in input |> In_channel.input_lines |> target_challenge
