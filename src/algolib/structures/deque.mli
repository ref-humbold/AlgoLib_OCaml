(** Double-ended queue structure. *)

type 'a t
(** The type of deques containing elements of type ['a]. *)

exception EmptyDeque
(** Exception raised when retrieving or removing elements from empty deque. *)

val empty : 'a t
(** The empty deque. *)

val is_empty : 'a t -> bool
(** [is_empty d] checks if deque [d] is empty. *)

val front : 'a t -> 'a
(** [front d] retrieves element from the front of deque [d].

    @raise EmptyDeque if deque is empty. *)

val back : 'a t -> 'a
(** [back d] retrieves element from the back of deque [d].

    @raise EmptyDeque if deque is empty. *)

val push_front : 'a -> 'a t -> 'a t
(** [push_front x d] adds element [x] at the front of deque [d]. *)

val ( @++ ) : 'a -> 'a t -> 'a t
(** [( @++ )] is an infix alias of [push_front]. Right associative. *)

val push_back : 'a t -> 'a -> 'a t
(** [push_back d x] adds element [x] at the back of deque [d]. *)

val ( &++ ) : 'a t -> 'a -> 'a t
(** [( &++ )] is an infix alias of [push_back]. Left associative. *)

val pop_front : 'a t -> 'a t
(** [pop_front d] removes element from the front element of deque [d].

    @raise EmptyDeque if deque is empty. *)

val pop_back : 'a t -> 'a t
(** [pop_back d] removes element from the back of deque [d].

    @raise EmptyDeque if deque is empty. *)
