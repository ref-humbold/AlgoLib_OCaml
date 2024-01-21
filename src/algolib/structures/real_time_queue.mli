type 'a t
(** The type of queues containing elements of type 'a. *)

exception Empty_queue
(** Exception raised when retrieving or removing elements from empty queue. *)

val empty : 'a t
(** The empty queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] checks if queue [q] is empty. *)

val front : 'a t -> 'a
(** [front q] retrieves element from the front of queue [q].

    @raise Empty_queue if queue is empty. *)

val push : 'a -> 'a t -> 'a t
(** [push x q] adds element [x] at the back of queue [q]. *)

val pop : 'a t -> 'a t
(** [pop q] removes element from the front of queue [q].

    @raise Empty_queue if queue is empty. *)
