(** Structure of point in a space *)

(** The type of points in a space. *)
type point3d = Pt3D of float * float * float

val pt3d : float -> float -> float -> point3d
(** [p3d x y z] creates a point [(x, y, z)]. *)

val pt3d_i : int -> int -> int -> point3d
(** [p3d_i x y z] creates a point [(x, y, z)]. *)

val radius : point3d -> float
(** [radius p] computes distance of point [p] from zero point. *)
