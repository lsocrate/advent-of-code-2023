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

let kind_hex = function
  | High_card -> '0'
  | One_pair -> '1'
  | Two_pair -> '2'
  | Three_of_a_kind -> '3'
  | Full_house -> '4'
  | Four_of_a_kind -> '5'
  | Five_of_a_kind -> '6'

type hand = { cards : string; kind : kind; rank_value : int; bid : int }
[@@deriving show]

let no_joker_hex = function
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

let joker_hex = function
  | 'J' -> '0'
  | '2' -> '1'
  | '3' -> '2'
  | '4' -> '3'
  | '5' -> '4'
  | '6' -> '5'
  | '7' -> '6'
  | '8' -> '7'
  | '9' -> '8'
  | 'T' -> '9'
  | 'Q' -> 'A'
  | 'K' -> 'B'
  | 'A' -> 'C'
  | _ -> failwith "bad  number"

let rank_number card_hex_fn cards kind =
  int_of_string
    ("0x" ^ Char.escaped (kind_hex kind) ^ String.map card_hex_fn cards)

let rec upgrade_hand_with_jokers js h =
  match (js, h) with
  | 0, _ -> h
  | _, Five_of_a_kind -> h
  | _, Full_house -> h
  | 1, Four_of_a_kind -> Five_of_a_kind
  | 1, Three_of_a_kind -> Four_of_a_kind
  | 1, Two_pair -> Full_house
  | 1, One_pair -> Three_of_a_kind
  | 1, High_card -> One_pair
  | n, Four_of_a_kind -> upgrade_hand_with_jokers (n - 1) Five_of_a_kind
  | n, Three_of_a_kind -> upgrade_hand_with_jokers (n - 1) Four_of_a_kind
  | n, Two_pair -> upgrade_hand_with_jokers (n - 1) Full_house
  | n, One_pair -> upgrade_hand_with_jokers (n - 1) Three_of_a_kind
  | n, High_card -> upgrade_hand_with_jokers (n - 1) One_pair

let into_hand_map hm c =
  HandMap.update c (function Some n -> Some (n + 1) | None -> Some 1) hm

let no_joker_kind_of_hand cards =
  String.fold_left into_hand_map HandMap.empty cards
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

let joker_kind_of_hand cards =
  let joker = 'J' in
  let jokers, non_jokers =
    String.fold_left into_hand_map HandMap.empty cards
    |> HandMap.partition (fun c _ -> c == joker)
  in
  let joker_count = HandMap.find_opt joker jokers |> Option.value ~default:0 in
  non_jokers |> HandMap.to_list |> List.map snd
  |> List.sort (fun x y -> -1 * compare x y)
  |> fun ns ->
  match ns with
  | 5 :: _ -> upgrade_hand_with_jokers joker_count Five_of_a_kind
  | 4 :: _ -> upgrade_hand_with_jokers joker_count Four_of_a_kind
  | 3 :: 2 :: _ -> upgrade_hand_with_jokers joker_count Full_house
  | 3 :: _ -> upgrade_hand_with_jokers joker_count Three_of_a_kind
  | 2 :: 2 :: _ -> upgrade_hand_with_jokers joker_count Two_pair
  | 2 :: _ -> upgrade_hand_with_jokers joker_count One_pair
  | _ -> upgrade_hand_with_jokers joker_count High_card

let hand_of_line kind_fn card_hex_fn line =
  match String.split_on_char ' ' line with
  | cards :: bid :: _ ->
      let kind = kind_fn cards in
      let bid = int_of_string bid in
      { cards; kind; bid; rank_value = rank_number card_hex_fn cards kind }
  | _ -> failwith @@ "bad hand: " ^ line

let challenge1 input =
  String.split_on_char '\n' input
  |> List.to_seq
  |> Seq.filter (fun l -> String.length l > 0)
  |> Seq.map (hand_of_line no_joker_kind_of_hand no_joker_hex)
  |> List.of_seq
  |> List.sort (fun x y -> x.rank_value - y.rank_value)
  |> List.mapi (fun idx h -> h.bid * (idx + 1))
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let challenge2 input =
  String.split_on_char '\n' input
  |> List.to_seq
  |> Seq.filter (fun l -> String.length l > 0)
  |> Seq.map (hand_of_line joker_kind_of_hand joker_hex)
  |> List.of_seq
  |> List.sort (fun x y -> x.rank_value - y.rank_value)
  |> List.mapi (fun idx h -> h.bid * (idx + 1))
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline

let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
