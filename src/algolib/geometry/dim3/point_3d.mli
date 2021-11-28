(** Structure of point in 3D *)

(** The type of points in 3D. *)
type point3d = Point3D of float * float * float

val pt3d : float -> float -> float -> point3d
(** [p3d x y z] creates a point [(x, y, z)]. *)

val pt3d_i : int -> int -> int -> point3d
(** [p3d_i x y z] creates a point [(x, y, z)]. *)

val coordinates : point3d -> float * float * float
(** [coordinates p] returns tuple of point coordinates. *)

val coordinates_list : point3d -> float list
(** [coordinates_list p] returns list of point coordinates. *)

val equal : point3d -> point3d -> bool
(** [equal p1 p2] checks whether points [p1] and [p2] are equal. *)

val radius : point3d -> float
(** [radius p] computes distance of point [p] from zero point. *)
