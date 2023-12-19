let ints_of_line line =
  String.split_on_char ' ' line |> List.filter_map int_of_string_opt

let solve_quadratic a b c =
  let delta = sqrt (Float.pow b 2. -. (4. *. a *. c)) in
  let x1 = (-.b +. delta) /. (2. *. a) in
  let x2 = (-.b -. delta) /. (2. *. a) in
  if x1 < x2 then (x1, x2) else (x2, x1)

let rec greatest_common_divisor a = function
  | 0 -> a
  | b -> greatest_common_divisor b (a mod b)

let least_common_multiple a b =
  if a = 0 || b = 0 then 0 (* LCM is undefined for zero *)
  else abs (a * b) / greatest_common_divisor a b
