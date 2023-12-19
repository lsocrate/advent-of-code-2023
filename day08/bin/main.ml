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
      let nxt_lbl = step inst sc (NodeMap.find current nodes) in
      find_zzz nodes inst (sc + 1) nxt_lbl

let rec find_final_z nodes inst sc lbl =
  if String.ends_with ~suffix:"Z" lbl then sc
  else
    let nxt_lbl = step inst sc (NodeMap.find lbl nodes) in
    find_final_z nodes inst (sc + 1) nxt_lbl

let challenge1 lines =
  let instructions, nodes = instructions_and_nodes lines in
  find_zzz nodes instructions 0 "AAA" |> string_of_int |> print_endline

let challenge2 lines =
  let instructions, nodes = instructions_and_nodes lines in
  NodeMap.to_list nodes
  |> List.filter_map (fun (l, _) ->
         if String.ends_with ~suffix:"A" l then Some l else None)
  |> List.map (find_final_z nodes instructions 0)
  |> List.fold_left Utils.least_common_multiple 1
  |> string_of_int |> print_endline

let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
