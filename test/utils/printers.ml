let list printer lst = Printf.sprintf "[%s]" @@ String.concat "; " @@ List.map printer lst

let tuple2 printer1 printer2 (p1, p2) = Printf.sprintf "(%s, %s)" (printer1 p1) (printer2 p2)

let tuple3 printer1 printer2 printer3 (p1, p2, p3) =
  Printf.sprintf "(%s, %s, %s)" (printer1 p1) (printer2 p2) (printer3 p3)

let option printer opt =
  match opt with
  | Some x -> Printf.sprintf "Some %s" @@ printer x
  | None -> "None"
