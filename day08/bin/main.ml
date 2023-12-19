type node = { left : string; right : string }

module NodeMap = Map.Make (String)

type instructions = { length : int; steps : (node -> string) array }

let step ins n = Array.get ins.steps (n mod ins.length)

let step_of_char = function
  | 'R' -> fun n -> n.right
  | 'L' -> fun n -> n.left
  | _ -> failwith "bad char"

let node_of_line line =
  let label = String.sub line 0 3 in
  let left = String.sub line 7 3 in
  let right = String.sub line 12 3 in
  (label, { left; right })

let instructions_and_nodes = function
  | instructions :: _ :: nodes ->
      let steps =
        String.to_seq instructions |> Seq.map step_of_char |> Array.of_seq
      in
      let node_map = List.map node_of_line nodes |> NodeMap.of_list in
      ({ steps; length = Array.length steps }, node_map)
  | _ -> failwith "bad parse"

let rec find_zzz nodes inst sc = function
  | "ZZZ" -> sc
  | current ->
      let node = NodeMap.find current nodes in
      let nxt_lbl = step inst sc node in
      find_zzz nodes inst (sc + 1) nxt_lbl

let challenge1 lines =
  let instructions, nodes = instructions_and_nodes lines in
  find_zzz nodes instructions 0 "AAA" |> string_of_int |> print_endline

let challenge2 input = print_endline "No yet"
let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
