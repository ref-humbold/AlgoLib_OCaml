(** Structure of point in 3D *)

(** The type of points in 3D. *)
type point3d = Point3D of float * float * float

val pt3d : float -> float -> float -> point3d
(** [p3d x y z] creates a point [(x, y, z)]. *)

val pt3d_i : int -> int -> int -> point3d
(** [p3d_i x y z] creates a point [(x, y, z)]. *)

val coordinates : point3d -> float * float * float
(** [coordeinates p] returns tuple of point coordinates. *)

val radius : point3d -> float
(** [radius p] computes distance of point [p] from zero point. *)
