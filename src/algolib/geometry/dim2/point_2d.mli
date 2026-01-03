include module type of Geometry_object

(** The type of points in 2D. *)
type t = Point2D of float * float

val pt2d : float -> float -> t
(** [pt2d x y] creates a point [(x, y)]. *)

val pt2d_i : int -> int -> t
(** [pt2d_i x y] creates a point [(x, y)]. *)

val coordinates : t -> float * float
(** [coordinates p] returns tuple of point coordinates. *)

val coordinates_list : t -> float list
(** [coordinates_list p] returns list of point coordinates. *)

val equal : t -> t -> bool
(** [equal p1 p2] checks whether points [p1] and [p2] are equal. *)

val radius : t -> float
(** [radius p] computes distance of point [p] from zero point. *)

val angle_rad : t -> float
(** [angle_rad p] computes angle in radians of point [p] between its radius and X axis. Value of the
    angle is between -PI exclusive and PI inclusive. *)

val angle_deg : t -> float
(** [angle_deg p] computes angle in degrees of point [p] between its radius and X axis. Value of the
    angle is between 0 inclusive and 360 exclusive. *)

val to_string : t -> string
(** [to_string p] returns string representation of point [p]. *)
