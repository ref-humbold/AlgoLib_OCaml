let int_list lst = "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
