type 'a t
(** The type of deques containing elements of type ['a]. *)

exception Empty_deque
(** Exception raised when retrieving or removing elements from empty deque. *)

val empty : 'a t
(** The empty deque. *)

val is_empty : 'a t -> bool
(** [is_empty d] checks if deque [d] is empty. *)

val length : 'a t -> int
(** [length d] return number of element in deque [d]. *)

val front : 'a t -> 'a
(** [front d] retrieves element from the front of deque [d].

    @raise Empty_deque if deque is empty. *)

val back : 'a t -> 'a
(** [back d] retrieves element from the back of deque [d].

    @raise Empty_deque if deque is empty. *)

val push_front : 'a -> 'a t -> 'a t
(** [push_front x d] adds element [x] at the front of deque [d]. *)

val push_back : 'a -> 'a t -> 'a t
(** [push_back x d] adds element [x] at the back of deque [d]. *)

val pop_front : 'a t -> 'a t
(** [pop_front d] removes element from the front element of deque [d].

    @raise Empty_deque if deque is empty. *)

val pop_back : 'a t -> 'a t
(** [pop_back d] removes element from the back of deque [d].

    @raise Empty_deque if deque is empty. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq xs] creates new deque with elements of Seq [xs]. Head of [xs] becomes front of created
    deque. *)

val of_list : 'a list -> 'a t
(** [of_list xs] creates new deque with elements of list [xs]. Head of [xs] becomes front of created
    deque. *)
