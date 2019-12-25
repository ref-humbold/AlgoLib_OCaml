let string_of_int_list lst =
  "[" ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_int x) "" lst ^ "]"
