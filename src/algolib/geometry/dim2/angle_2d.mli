type t
(** The type of angles in 2D. *)

val angle_deg : float -> t
(** [angle_deg d] creates angle with value of [d] degrees. *)

val angle_rad : float -> t
(** [angle_rad r] creates angle with value of [r] radians. *)

val degrees : t -> float
(** [degrees a] returns value of angle [a] in degrees between 0 inclusive and 360 exclusive. *)

val radians : t -> float
(** [radians a] returns value of angle [a] in radians between 0 inclusive and 2 * PI exclusive. *)

val compare : t -> t -> int
(** [compare a1 a2] compares angles [a1] and [a2]. *)

val to_string : t -> string
(** [to_string a] returns string representation of angle [a]. *)
