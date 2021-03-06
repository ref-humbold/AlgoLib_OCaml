(* Structure of vector on a plane *)
type vector2d = Vec2D of float * float

let vec2d x y = Vec2D (x, y)

let vec2d_i x y = Vec2D (float_of_int x, float_of_int y)

let length (Vec2D (x, y)) = sqrt ((x *. x) +. (y *. y))

let ( +$ ) (Vec2D (x1, y1)) (Vec2D (x2, y2)) = Vec2D (x1 +. x2, y1 +. y2)

let ( -$ ) (Vec2D (x1, y1)) (Vec2D (x2, y2)) = Vec2D (x1 -. x2, y1 -. y2)

let ( *$ ) (Vec2D (x, y)) c = Vec2D (x *. c, y *. c)

let ( /$ ) (Vec2D (x, y)) c = if c = 0.0 then raise Division_by_zero else Vec2D (x /. c, y /. c)

let dot (Vec2D (x1, y1)) (Vec2D (x2, y2)) = (x1 *. x2) +. (y1 *. y2)

let area (Vec2D (x1, y1)) (Vec2D (x2, y2)) = (x1 *. y2) -. (x2 *. y1)
