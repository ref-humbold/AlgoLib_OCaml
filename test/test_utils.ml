module Printers = struct
  let list printer lst = "[" ^ String.concat "; " (List.map printer lst) ^ "]"

  let tuple2 printer1 printer2 (p1, p2) = "(" ^ printer1 p1 ^ ", " ^ printer2 p2 ^ ")"

  let tuple3 printer1 printer2 printer3 (p1, p2, p3) =
    "(" ^ printer1 p1 ^ ", " ^ printer2 p2 ^ ", " ^ printer3 p3 ^ ")"

  let option printer opt =
    match opt with
    | Some x -> "Some " ^ printer x
    | None -> "None"
end

module Messages = struct
  let true_value = "Expected true value"

  let false_value = "Expected false value"
end
