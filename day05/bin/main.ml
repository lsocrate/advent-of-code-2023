type transform_function = int -> int [@@deriving show]
type transform_range = { min : int; max : int; transform : transform_function }

let in_range n range = n >= range.min && n <= range.max

type instruction = {
  seeds : int list;
  seed_to_soil : transform_function;
  soil_to_fertilizer : transform_function;
  fertilizer_to_water : transform_function;
  water_to_light : transform_function;
  light_to_temperature : transform_function;
  temperature_to_humidity : transform_function;
  humidity_to_location : transform_function;
}
[@@deriving show]

let ints_of_line line =
  String.split_on_char ' ' line |> List.filter_map int_of_string_opt

let read_seeds line =
  let header = "seeds: " in
  let header_length = String.length header in
  if not @@ String.starts_with ~prefix:header line then None
  else
    String.sub line header_length (String.length line - header_length)
    |> ints_of_line |> Option.some

let range_of_line s =
  match ints_of_line s with
  | [ dest; source; range ] ->
      Some
        {
          min = source;
          max = source + range;
          transform = (fun n -> n - source + dest);
        }
  | _ -> None

let consolidate_ranges ranges n =
  List.find_opt (in_range n) ranges
  |> Option.map (fun r -> r.transform n)
  |> Option.value ~default:n

let read_range_map name str =
  if not @@ String.starts_with ~prefix:name str then None
  else
    String.split_on_char '\n' str
    |> List.tl
    |> List.filter_map range_of_line
    |> consolidate_ranges |> Option.some

let rec to_instructions seeds seed_to_soil soil_to_fertilizer
    fertilizer_to_water water_to_light light_to_temperature
    temperature_to_humidity humidity_to_location lines =
  match lines with
  | [] ->
      {
        seeds;
        seed_to_soil;
        soil_to_fertilizer;
        fertilizer_to_water;
        water_to_light;
        light_to_temperature;
        temperature_to_humidity;
        humidity_to_location;
      }
  | l :: ls ->
      to_instructions
        (Option.value ~default:seeds (read_seeds l))
        (Option.value ~default:seed_to_soil (read_range_map "seed-to-soil" l))
        (Option.value ~default:soil_to_fertilizer
           (read_range_map "soil-to-fertilizer" l))
        (Option.value ~default:fertilizer_to_water
           (read_range_map "fertilizer-to-water" l))
        (Option.value ~default:water_to_light
           (read_range_map "water-to-light" l))
        (Option.value ~default:light_to_temperature
           (read_range_map "light-to-temperature" l))
        (Option.value ~default:temperature_to_humidity
           (read_range_map "temperature-to-humidity" l))
        (Option.value ~default:humidity_to_location
           (read_range_map "humidity-to-location" l))
        ls

let soil_of_seed i seed =
  seed |> i.seed_to_soil |> i.soil_to_fertilizer |> i.fertilizer_to_water
  |> i.water_to_light |> i.light_to_temperature |> i.temperature_to_humidity
  |> i.humidity_to_location

let challenge1 input =
  let to_lower_location i lower_location s =
    min lower_location (soil_of_seed i s)
  in
  Str.split (Str.regexp_string "\n\n") input
  |> to_instructions [] Fun.id Fun.id Fun.id Fun.id Fun.id Fun.id Fun.id
  |> (fun i ->
       match i.seeds with
       | [] -> failwith "No Seeds!"
       | first :: rest ->
           List.fold_left (to_lower_location i) (soil_of_seed i first) rest)
  |> string_of_int |> print_endline

let unwrapped_seed_sequence_of_seeds = function
  | seed :: count :: rest ->
      let aux (s, c, r) =
        if c > 1 then Some (s, (s + 1, c - 1, r))
        else if c == 1 then
          match r with
          | new_seed :: new_count :: new_rest ->
              Some (s, (new_seed, new_count, new_rest))
          | _ -> None
        else None
      in
      Seq.unfold aux (seed, count, rest)
  | _ -> failwith "Bad seeds"

let challenge2 input =
  let to_lower_location i lower_location s =
    min lower_location (soil_of_seed i s)
  in
  Str.split (Str.regexp_string "\n\n") input
  |> to_instructions [] Fun.id Fun.id Fun.id Fun.id Fun.id Fun.id Fun.id
  |> (fun i ->
       Seq.fold_left (to_lower_location i) 999999999999999999
         (unwrapped_seed_sequence_of_seeds i.seeds))
  |> string_of_int |> print_endline

let () = Challenge.run_challenge { easy = challenge1; hard = challenge2 }
