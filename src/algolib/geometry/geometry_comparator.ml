module type GEOMETRY_COMPARATOR = sig
  val compare : float -> float -> int
end

module GeometryComparator : GEOMETRY_COMPARATOR = struct
  let epsilon = 1e-12

  let compare f1 f2 = if abs_float (f1 -. f2) < epsilon then 0 else if f1 < f2 then -1 else 1
end
