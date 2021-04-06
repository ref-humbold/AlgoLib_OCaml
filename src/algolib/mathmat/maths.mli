(** Algorithms for basic mathematical computations *)

val gcd : int -> int -> int
(** [gcd n1 n2] computes the greatest common divisor of two integers [n1] and [n2]. *)

val ( **/ ) : int -> int -> int
(** [( **/ )] is an infix alias of [gcd]. Right associative. *)

val lcm : int -> int -> int
(** [lcm n1 n2] computes the least common multiple of two integers [n1] and [n2]. *)

val ( **^ ) : int -> int -> int
(** [( **^ )] is an infix alias of [lcm]. Right associative. *)

val multiply_mod : ?modulo:int -> int -> int -> int
(** [multiply_mod ?m f1 f2] performs fast integer multiplication of [f1] and [f2] taken modulo [m].

    @raise Failure if arithmetic error occurs. *)

val power_mod : ?modulo:int -> int -> int -> int
(** [power_mod ?m b e] performs fast integer exponentiation of [b] to the power of [e] taken modulo
    [m].

    @raise Failure if arithmetic error occurs. *)
