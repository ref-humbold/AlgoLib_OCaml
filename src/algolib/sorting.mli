(** SORTING ALGORITHMS *)

type point = float * float

val angle_sort: point list -> point list
(** [angle_sort lst] sorts points [lst] on a plane with respect to the angle *)

val merge_sort: 'a list -> 'a list
(** [merge_sort lst] sorts sequence [lst] using merge sort algorithm *)