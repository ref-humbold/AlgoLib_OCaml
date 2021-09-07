(* Algorithms for basic geometrical computations in 3D *)

let sort_by_x lst =
  List.stable_sort
    (fun (Point_3d.Point3D (x1, _, _)) (Point_3d.Point3D (x3, _, _)) -> compare x1 x3)
    lst

let sort_by_y lst =
  List.stable_sort
    (fun (Point_3d.Point3D (_, y1, _)) (Point_3d.Point3D (_, y3, _)) -> compare y1 y3)
    lst

let sort_by_z lst =
  List.stable_sort
    (fun (Point_3d.Point3D (_, _, z1)) (Point_3d.Point3D (_, _, z3)) -> compare z1 z3)
    lst

let distance (Point_3d.Point3D (p1x, p1y, p1z)) (Point_3d.Point3D (p3x, p3y, p3z)) =
  sqrt
    ( ((p3x -. p1x) *. (p3x -. p1x))
      +. ((p3y -. p1y) *. (p3y -. p1y))
      +. ((p3z -. p1z) *. (p3z -. p1z)) )

let translate (Point_3d.Point3D (px, py, pz)) (Vector_3d.Vector3D (vx, vy, vz)) =
  Point_3d.pt3d (px +. vx) (py +. vy) (pz +. vz)

let reflect (Point_3d.Point3D (px, py, pz)) (Point_3d.Point3D (cx, cy, cz)) =
  Point_3d.pt3d (-.px +. cx +. cx) (-.py +. cy +. cy) (-.pz +. cz +. cz)
