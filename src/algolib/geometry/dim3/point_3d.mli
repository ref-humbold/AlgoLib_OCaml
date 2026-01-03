include module type of Geometry_object

(** The type of points in 3D. *)
type t = Point3D of float * float * float

val pt3d : float -> float -> float -> t
(** [p3d x y z] creates a point [(x, y, z)]. *)

val pt3d_i : int -> int -> int -> t
(** [p3d_i x y z] creates a point [(x, y, z)]. *)

val coordinates : t -> float * float * float
(** [coordinates p] returns tuple of point coordinates. *)

val coordinates_list : t -> float list
(** [coordinates_list p] returns list of point coordinates. *)

val equal : t -> t -> bool
(** [equal p1 p2] checks whether points [p1] and [p2] are equal. *)

val radius : t -> float
(** [radius p] computes distance of point [p] from zero point. *)

val to_string : t -> string
(** [to_string p] returns string representation of point [p]. *)
