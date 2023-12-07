open Fun

(* SCHEMA *)
type schema = { input : string; width : int }

(* POSITION *)
(* x,y where left top = 0,0 *)
type position = Position of int * int [@@deriving show]

module Position = struct
  type t = position

  let compare = compare
end

module PositionMap = Map.Make (Position)
module PositionSet = Set.Make (Position)

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

type number = Connected of int * PositionSet.t | Disconnected of int

let number_init schema (pos : position) n =
  let gs, ngs =
    positions_around pos
    |> List.fold_left
         (fun cs p ->
           match (char_in_schema schema p, cs) with
           | None, _ -> cs
           | Some ('0' .. '9' | '.' | '\n'), _ -> cs
           | Some '*', (gs, ngs) -> (PositionSet.add p gs, ngs)
           | Some _, (gs, ngs) -> (gs, PositionSet.add p ngs))
         (PositionSet.empty, PositionSet.empty)
  in
  if PositionSet.is_empty gs && PositionSet.is_empty ngs then Disconnected n
  else Connected (n, gs)

(* GROUP *)
type group = {
  mutable value : int;
  mutable is_open : bool;
  mutable is_part : bool;
  mutable touches : PositionSet.t;
}

let group_init = function
  | Disconnected n ->
      {
        value = n;
        is_open = true;
        is_part = false;
        touches = PositionSet.empty;
      }
  | Connected (n, gs) ->
      { value = n; is_open = true; is_part = true; touches = gs }

let close_group g =
  g.is_open <- false;
  g

let push_number_into_group group number =
  let push g n = (g * 10) + n in
  if not group.is_open then failwith "adding to closed group"
  else
    match number with
    | Disconnected n ->
        group.value <- push group.value n;
        group
    | Connected (n, gs) ->
        group.value <- push group.value n;
        group.is_part <- true;
        group.touches <- PositionSet.union gs group.touches;
        group

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
                if prev.is_open then push_number_into_group prev n :: rest
                else group_init n :: prev :: rest)
        | _ -> (
            match res with prev :: rest -> close_group prev :: rest | _ -> res)
      in
      process input res (idx + 1)

let challenge1 input =
  let sum_parts t g = if g.is_part then t + g.value else t in
  process input [] 0 |> List.fold_left sum_parts 0 |> string_of_int
  |> print_endline

let challenge2 input =
  let cons n = function None -> Some [ n ] | Some ns -> Some (n :: ns) in
  let into_map_of_gears gm g =
    if not g.is_part then gm
    else PositionSet.fold (flip PositionMap.update (cons g.value)) g.touches gm
  in
  let sum_positions_with_two_numbers_with_multiples _ = function
    | [ x; y ] -> ( + ) (x * y)
    | _ -> id
  in
  process input [] 0
  |> List.fold_left into_map_of_gears PositionMap.empty
  |> flip (PositionMap.fold sum_positions_with_two_numbers_with_multiples) 0
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
  open_in input |> In_channel.input_all |> target_challenge
