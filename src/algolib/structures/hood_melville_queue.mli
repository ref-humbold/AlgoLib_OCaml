type 'a t
val empty: 'a t
val is_empty: 'a t -> bool
val get: 'a t -> 'a
val push: 'a -> 'a t -> 'a t
val pop: 'a t -> 'a t
