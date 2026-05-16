module type GEOMETRY_COMPARATOR = sig
  val compare : float -> float -> int
end

module GeometryComparator : GEOMETRY_COMPARATOR
