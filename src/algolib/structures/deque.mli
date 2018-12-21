(** DOUBLE-ENDED QUEUE STRUCTURE *)
(** The type of deques containing elements of type 'a. *)
type 'a t

(** Exception raised when retrieving or removing elements from empty deque. *)
exception EmptyDeque

(** [create ()] returns a new deque. *)
val create: unit -> 'a t

(** [is_empty d] checks if deque [d] is empty. *)
val is_empty: 'a t -> bool

(** [front d] retrieves element from the front of deque [d]. *)
val front: 'a t -> 'a

(** [back d] retrieves element from the back of deque [d]. *)
val back: 'a t -> 'a

(** [push_front x d] adds element [x] at the front of deque [d]. *)
val push_front: 'a -> 'a t -> 'a t

(** [push_back x d] adds element [x] at the back of deque [d]. *)
val push_back: 'a -> 'a t -> 'a t

(** [pop_front d] removes element from the front element of deque [d]. *)
val pop_front: 'a t -> 'a t

(** [pop_back d] removes element from the back of deque [d]. *)
val pop_back: 'a t -> 'a t
