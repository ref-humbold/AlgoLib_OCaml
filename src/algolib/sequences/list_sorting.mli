(** Algorithms for list sorting *)

val merge_sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** [merge_sort cmp lst] sorts list [lst] according to comparison function [cmp] using merge sort
    algorithm. *)
