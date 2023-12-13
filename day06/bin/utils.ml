let ints_of_line line =
  String.split_on_char ' ' line |> List.filter_map int_of_string_opt
