(** Structure of point in 2D *)

(** The type of points in 2D. *)
type point2d = Point2D of float * float

val pt2d : float -> float -> point2d
(** [pt2d x y] creates a point [(x, y)]. *)

val pt2d_i : int -> int -> point2d
(** [pt2d_i x y] creates a point [(x, y)]. *)

val coordinates : point2d -> float * float
(** [coordinates p] returns tuple of point coordinates. *)

val coordinates_list : point2d -> float list
(** [coordinates_list p] returns list of point coordinates. *)

val equal : point2d -> point2d -> bool
(** [equal p1 p2] checks whether points [p1] and [p2] are equal. *)

val radius : point2d -> float
(** [radius p] computes distance of point [p] from zero point. *)

val angle_rad : point2d -> float
(** [angle_rad p] computes angle in radians of point [p] between its radius and X axis. Value of the
    angle is between -PI exclusive and PI inclusive. *)

val angle_deg : point2d -> float
(** [angle_deg p] computes angle in degrees of point [p] between its radius and X axis. Value of the
    angle is between 0 inclusive and 360 exclusive. *)
