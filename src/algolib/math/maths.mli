(** Basic mathematics algorithms *)

val gcdiv: int -> int -> int
(** [gcdiv n1 n2] computes the greatest common divisor of two non-negative integers [n1] and [n2] *)

val lcm: int -> int -> int
(** [lcm n1 n2] computes the least common multiple of two non-negative integers [n1] and [n2] *)

val mult_mod: int -> int -> int -> int
(** [mult_mod f1 f2 m] performs fast integer multiplication of [f1] and [f2] taken modulo [m] *)

val power_mod: int -> int -> int -> int
(** [power_mod b e m] performs fast integer exponentiation of [b] to the power of [e] taken modulo [m] *)
