(** Algorithms for prime numbers *)

val test_fermat: int -> bool
(** [test_fermat n] checks if [n] is prime using Fermat prime test *)

val test_miller: int -> bool
(** [test_miller n] checks if [n] is prime using Rabin-Miller prime test *)
