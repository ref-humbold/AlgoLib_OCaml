(** REAL-TIME HOOD-MELVILLE QUEUE STRUCTURE *)
(** The type of queues containing elements of type 'a. *)
type 'a t

(** Exception raised when retrieving or removing elements from empty queue. *)
exception EmptyQueue

(** [create ()] returns a new queue. *)
val create: unit -> 'a t

(** [is_empty q] checks if queue [q] is empty. *)
val is_empty: 'a t -> bool

(** [front q] retrieves element from the front of queue [q]. *)
val front: 'a t -> 'a

(** [push x d] adds element [x] at the back of queue [q]. *)
val push: 'a -> 'a t -> 'a t

(** [pop q] removes element from the front of queue [q]. *)
val pop: 'a t -> 'a t
