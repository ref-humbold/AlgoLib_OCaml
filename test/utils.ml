module Printers = struct
  let list_str printer lst = "[" ^ String.concat "; " (List.map printer lst) ^ "]"

  let opt_str printer opt =
    match opt with
    | Some x -> "Some " ^ printer x
    | None -> "None"
end

module Messages = struct
  let true_value = "Expected true value"

  let false_value = "Expected false value"
end
