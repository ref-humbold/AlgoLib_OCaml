type t
(** The type of angles in 2D. *)

val from_degrees : float -> t
(** [from_degrees d] creates angle with value of [d] degrees. *)

val from_radians : float -> t
(** [from_radians r] creates angle with value of [r] radians. *)

val degrees : t -> float
(** [degrees a] returns value of angle [a] in degrees between 0 inclusive and 360 exclusive. *)

val radians : t -> float
(** [radians a] returns value of angle [a] in radians between 0 inclusive and 2 * PI exclusive. *)

val compare : t -> t -> int
(** [compare a1 a2] compares angles [a1] and [a2]. *)

val equal : t -> t -> bool
(** [equal a1 a2] checks whether angles [a1] and [a2] are equal. *)

val to_string : t -> string
(** [to_string a] returns string representation of angle [a]. *)
