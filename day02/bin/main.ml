open In_channel

type game = {
  id : int;
  mutable max_red : int;
  mutable max_green : int;
  mutable max_blue : int;
}
[@@deriving show]

let game_init id = { id; max_red = 0; max_green = 0; max_blue = 0 }

let parts_of_line line =
  match String.split_on_char ':' line with
  | id_side :: draw_side :: _ -> (id_side, draw_side)
  | _ -> failwith "bad line"

let tail_at n str = String.sub str n @@ (String.length str - n)
let id_of_draw str_left = int_of_string @@ tail_at 5 str_left

let rgb_of_draw draw =
  let to_rgb str =
    match String.trim str |> String.split_on_char ' ' with
    | n :: "red" :: _ -> (int_of_string n, 0, 0)
    | n :: "green" :: _ -> (0, int_of_string n, 0)
    | n :: "blue" :: _ -> (0, 0, int_of_string n)
    | _ -> failwith "Bad RGB"
  in
  let merge_max (old_r, old_g, old_b) reveal =
    let r, g, b = to_rgb reveal in
    (max r old_r, max g old_g, max b old_b)
  in
  String.split_on_char ',' draw |> List.fold_left merge_max (0, 0, 0)

let is_game_possible (r, g, b) game =
  game.max_red <= r && game.max_green <= g && game.max_blue <= b

let game_power game = game.max_red * game.max_green * game.max_blue

let consolidate_draws game draw =
  let r, g, b = rgb_of_draw draw in
  game.max_red <- max game.max_red r;
  game.max_green <- max game.max_green g;
  game.max_blue <- max game.max_blue b;
  game

let to_game line =
  let id_side, draw_side = parts_of_line line in
  let game : game = game_init @@ id_of_draw id_side in
  draw_side |> String.split_on_char ';' |> List.fold_left consolidate_draws game

let challenge1 lines =
  lines |> List.map to_game
  |> List.filter (is_game_possible (12, 13, 14))
  |> List.fold_left (fun t g -> t + g.id) 0
  |> string_of_int |> print_endline

let challenge2 lines =
  lines |> List.map to_game |> List.map game_power |> List.fold_left ( + ) 0
  |> string_of_int |> print_endline

let () =
  let input =
    match Sys.argv.(1) with
    | "test" -> "./test.input"
    | "puzzle" -> "./puzzle.input"
    | _ -> failwith "Invalid input file"
  in
  let target_challenge =
    match Sys.argv.(2) with
    | "easy" -> challenge1
    | "hard" -> challenge2
    | _ -> failwith "Choose level"
  in
  open_in input |> input_lines |> target_challenge
