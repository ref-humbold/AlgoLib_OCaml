let epsilon = 1e-12

let coordinates_equal c1 c2 =
  let float_equal f1 f2 = abs_float (f1 -. f2) < epsilon in
  List.for_all2 float_equal c1 c2
