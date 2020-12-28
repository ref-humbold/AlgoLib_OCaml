module Printers = struct
  let list printer lst = "[" ^ String.concat "; " (List.map printer lst) ^ "]"

  let option printer opt =
    match opt with
    | Some x -> "Some " ^ printer x
    | None -> "None"
end

module Messages = struct
  let true_value = "Expected true value"

  let false_value = "Expected false value"
end
