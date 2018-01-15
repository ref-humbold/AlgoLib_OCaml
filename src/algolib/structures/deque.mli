type 'a t
val empty: 'a t
val is_empty: 'a t -> bool
val get_front: 'a t -> 'a
val get_back: 'a t -> 'a
val push_front: 'a -> 'a t -> 'a t
val push_back: 'a -> 'a t -> 'a t
val pop_front: 'a t -> 'a t
val pop_back: 'a t -> 'a t
