(* SCHEMA *)
type schema = { input : string; width : int }

(* POSITION *)
(* x,y where left top = 0,0 *)
type position = Position of int * int [@@deriving show]

let schema_init input = { input; width = String.index input '\n' }

let read_idx schema idx =
  try Some (String.get schema.input idx) with Invalid_argument _ -> None

let char_in_schema schema pos =
  let (Position (x, y)) = pos in
  read_idx schema @@ (x + (y * (schema.width + 1)))

let position_init (x, y) = Position (x, y)

let position_of_idx schema idx =
  let length_with_break = schema.width + 1 in
  Position (idx mod length_with_break, idx / length_with_break)

let positions_around center =
  let Position (x,y) = center
  in
  List.map position_init
  [ (x - 1, y - 1);  (x, y - 1);  (x + 1, y - 1);
    (x - 1, y    );  (*(x, y);*)  (x + 1, y    );
    (x - 1, y + 1);  (x, y + 1);  (x + 1, y + 1);
  ] [@@ocamlformat "disable"]

(* NUMBERS *)

type number = Connected of int | Disconnected of int [@@deriving show]

let number_init schema (pos : position) n =
  let is_connection a =
    match a with '0' .. '9' | '.' | '\n' -> false | _ -> true
  in
  let is_connected =
    positions_around pos
    |> List.map (char_in_schema schema)
    |> List.exists (fun c ->
           c |> Option.map is_connection |> Option.value ~default:false)
  in

  if is_connected then Connected n else Disconnected n

(* GROUP *)
type group =
  | OpenPart of int
  | OpenNotPart of int
  | ClosedPart of int
  | ClosedNotPart of int
[@@deriving show]

let group_init = function
  | Connected n -> OpenPart n
  | Disconnected n -> OpenNotPart n

let is_group_open = function
  | OpenPart _ -> true
  | OpenNotPart _ -> true
  | _ -> false

let close_group g =
  match g with
  | OpenPart n -> ClosedPart n
  | OpenNotPart n -> ClosedNotPart n
  | _ -> g

let push_number_into_group group number =
  let push g n = (g * 10) + n in
  match (group, number) with
  | ClosedPart _, _ -> failwith "adding to closed group"
  | ClosedNotPart _, _ -> failwith "adding to closed group"
  | OpenNotPart g, Disconnected n -> OpenNotPart (push g n)
  | OpenNotPart g, Connected n -> OpenPart (push g n)
  | OpenPart g, Connected n -> OpenPart (push g n)
  | OpenPart g, Disconnected n -> OpenPart (push g n)

let rec process input res idx =
  let schema = schema_init input in
  match read_idx schema idx with
  | None -> (
      (* We are done, close last group and return *)
      match res with prev :: rest -> close_group prev :: rest | _ -> res)
  | Some char ->
      let res =
        match char with
        | '0' .. '9' -> (
            let pos = position_of_idx schema idx in
            let n = number_init schema pos (int_of_char char - 48) in
            match res with
            | [] -> [ group_init n ]
            | prev :: rest ->
                if is_group_open prev then push_number_into_group prev n :: rest
                else group_init n :: prev :: rest)
        | _ -> (
            match res with prev :: rest -> close_group prev :: rest | _ -> res)
      in
      process input res (idx + 1)

let challenge1 input =
  let sum_parts t g = match g with ClosedPart n -> t + n | _ -> t in
  process input [] 0 |> List.fold_left sum_parts 0 |> string_of_int
  |> print_endline

let challenge2 _schema = ()

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
  open_in input |> In_channel.input_all |> target_challenge
