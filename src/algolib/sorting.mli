(** Sorting algorithms. *)

type point = float * float

val angle_sort : point list -> point list
(** [angle_sort lst] sorts points [lst] on a plane with respect to the angle. *)

val merge_sort : ('a -> 'a -> bool) -> 'a list -> 'a list
(** [merge_sort cmp lst] sorts list [lst] according to comparison function [cmp] using merge sort
    algorithm. *)
