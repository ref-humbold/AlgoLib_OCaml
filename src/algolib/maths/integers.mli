val gcd : int -> int -> int
(** [gcd n1 n2] computes the greatest common divisor of two integers [n1] and [n2]. *)

val ( **/ ) : int -> int -> int
(** [n1 **/ n2] is [gcd n1 n2]. Right associative. *)

val lcm : int -> int -> int
(** [lcm n1 n2] computes the least common multiple of two integers [n1] and [n2]. *)

val ( **^ ) : int -> int -> int
(** [n1 **^ n2] is [lcm n1 n2]. Right associative. *)

val multiply : int -> int -> int
(** [multiply f1 f2] performs fast integer multiplication of [f1] and [f2].

    @raise Failure if arithmetic error occurs. *)

val ( *! ) : int -> int -> int
(** [n1 *$ n2] is [multiply n1 n2]. Left associative. *)

val multiply_mod : int -> int -> int -> int
(** [multiply_mod f1 f2 m] performs fast integer multiplication of [f1] and [f2] taken modulo [m].

    @raise Failure if arithmetic error occurs. *)

val power : int -> int -> int
(** [power b e] performs fast integer exponentiation of [b] to the power of [e].

    @raise Failure if arithmetic error occurs. *)

val ( **! ) : int -> int -> int
(** [n1 **! n2] is [power n1 n2]. Right associative. *)

val power_mod : int -> int -> int -> int
(** [power_mod b e m] performs fast integer exponentiation of [b] to the power of [e] taken modulo
    [m].

    @raise Failure if arithmetic error occurs. *)
