module HandMap = Map.Make (Char)

type kind =
  | High_card
  | One_pair
  | Two_pair
  | Three_of_a_kind
  | Full_house
  | Four_of_a_kind
  | Five_of_a_kind
[@@deriving show]

type hand = { cards : string; kind : kind; rank_value : int; bid : int }
[@@deriving show]

let card_hex = function
  | '2' -> '0'
  | '3' -> '1'
  | '4' -> '2'
  | '5' -> '3'
  | '6' -> '4'
  | '7' -> '5'
  | '8' -> '6'
  | '9' -> '7'
  | 'T' -> '8'
  | 'J' -> '9'
  | 'Q' -> 'A'
  | 'K' -> 'B'
  | 'A' -> 'C'
  | _ -> failwith "bad  number"

let kind_hex = function
  | High_card -> '0'
  | One_pair -> '1'
  | Two_pair -> '2'
  | Three_of_a_kind -> '3'
  | Full_house -> '4'
  | Four_of_a_kind -> '5'
  | Five_of_a_kind -> '6'

let rank_number cards kind =
  int_of_string ("0x" ^ Char.escaped (kind_hex kind) ^ String.map card_hex cards)

let kind_of_hand cards =
  let aux hm c =
    HandMap.update c (function Some n -> Some (n + 1) | None -> Some 1) hm
  in
  String.fold_left aux HandMap.empty cards
  |> HandMap.to_list |> List.map snd
  |> List.sort (fun x y -> -1 * compare x y)
  |> fun ns ->
  match ns with
  | 5 :: _ -> Five_of_a_kind
  | 4 :: _ -> Four_of_a_kind
  | 3 :: 2 :: _ -> Full_house
  | 3 :: 1 :: _ -> Three_of_a_kind
  | 2 :: 2 :: _ -> Two_pair
  | 2 :: 1 :: _ -> One_pair
  | _ -> High_card

let hand_of_line line =
  match String.split_on_char ' ' line with
  | cards :: bid :: _ ->
      let kind = kind_of_hand cards in
      {
        cards;
        kind;
        rank_value = rank_number cards kind;
        bid = int_of_string bid;
      }
  | _ -> failwith @@ "bad hand: " ^ line

let challenge1 input =
  String.split_on_char '\n' input
  |> List.to_seq
  |> Seq.filter (fun l -> String.length l > 0)
  |> Seq.map hand_of_line |> List.of_seq
  |> List.sort (fun x y -> x.rank_value - y.rank_value)
  |> List.mapi (fun idx h -> h.bid * (idx + 1))
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let challenge2 _ = print_endline "not yet"
let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
