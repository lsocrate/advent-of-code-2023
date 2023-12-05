open In_channel

type game = {
  id : int;
  mutable max_red : int;
  mutable max_green : int;
  mutable max_blue : int;
}
(*[@@deriving show]*)

let parts_of_line line =
  let[@warning "-8"] (id_side :: draw_side :: _) =
    String.split_on_char ':' line
  in
  (id_side, draw_side)

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

let to_game line =
  let id_side, draw_side = parts_of_line line in
  let game : game =
    { id = id_of_draw id_side; max_red = 0; max_green = 0; max_blue = 0 }
  in
  draw_side |> String.split_on_char ';'
  |> List.fold_left
       (fun game draw ->
         let r, g, b = rgb_of_draw draw in
         game.max_red <- max game.max_red r;
         game.max_blue <- max game.max_blue b;
         game.max_green <- max game.max_green g;
         game)
       game

let () =
  open_in "./puzzle.input" |> input_lines |> List.map to_game
  |> List.filter (is_game_possible (12, 13, 14))
  |> List.fold_left (fun t g -> t + g.id) 0
  |> string_of_int |> print_endline
