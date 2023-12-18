let ints_of_line line =
  String.split_on_char ' ' line |> List.filter_map int_of_string_opt

let solve_quadratic a b c =
  let delta = sqrt (Float.pow b 2. -. (4. *. a *. c)) in
  let x1 = (-.b +. delta) /. (2. *. a) in
  let x2 = (-.b -. delta) /. (2. *. a) in
   if x1 < x2 then (x1, x2) else (x2, x1)
