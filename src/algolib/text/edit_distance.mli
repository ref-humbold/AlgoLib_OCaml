(** Algorithms for edit distance *)

val count_levenshtein
  :  ?insertion_cost:float ->
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
(** [count_levenshtein ~ic ~dc ~sc s1 s2] counts LCS edit distance from string [s1] to string [s2]
    having operation cost [ic] for insertion and [dc] for deletion. Default cost for each operation
    is [1.0].

    @raise Invalid_argument if any cost is negative. *)
