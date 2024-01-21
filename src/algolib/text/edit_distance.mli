val count_levenshtein :
  ?insertion_cost:float ->
  ?deletion_cost:float ->
  ?substitution_cost:float ->
  string ->
  string ->
  float
(** [count_levenshtein ~ic ~dc ~sc s1 s2] counts Levenshtein edit distance from string [s1] to
    string [s2] having operation cost [ic] for insertion, [dc] for deletion and [sc] for
    substitution. Default cost for each operation is [1.0].

    @raise Invalid_argument if any cost is negative. *)

val count_lcs : ?insertion_cost:float -> ?deletion_cost:float -> string -> string -> float
(** [count_lcs ~ic ~dc ~sc s1 s2] counts LCS edit distance from string [s1] to string [s2] having
    operation cost [ic] for insertion and [dc] for deletion. Default cost for each operation is
    [1.0].

    @raise Invalid_argument if any cost is negative. *)

val count_hamming : ?substitution_cost:float -> string -> string -> float
(** [count_hamming ~sc s1 s2] counts Hamming edit distance from string [s1] to string [s2] of equal
    length having operation cost [sc] for substitution. Default cost for each operation is [1.0].

    @raise Invalid_argument if cost is negative or strings have different length *)
