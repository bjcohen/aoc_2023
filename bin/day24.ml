open! Core

let test_input =
  {|19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3|}
;;

let parse input =
  String.strip input
  |> String.split_lines
  |> List.map ~f:(fun l ->
    String.substr_replace_all l ~pattern:" " ~with_:""
    |> String.split ~on:'@'
    |> function[@warning "-8"]
    | [ p; v ] ->
      let[@warning "-8"] [ x; y; z ] =
        String.split p ~on:',' |> List.map ~f:float_of_string
      in
      let[@warning "-8"] [ xv; yv; zv ] =
        String.split v ~on:',' |> List.map ~f:float_of_string
      in
      (x, y, z), (xv, yv, zv))
;;

let cross2 (x1, y1) (x2, y2) = (x1 *. y2) -. (x2 *. y1)
let add2 (x1, y1) (x2, y2) = x1 +. x2, y1 +. y2
let sub2 (x1, y1) (x2, y2) = x1 -. x2, y1 -. y2
let mul2 (x1, y1) c = x1 *. c, y1 *. c
let vec3to2 = List.map ~f:(fun ((x1, y1, _), (x2, y2, _)) -> (x1, y1), (x2, y2))
let sub3 (x1, y1, z1) (x2, y2, z2) = x1 -. x2, y1 -. y2, z1 -. z2
let add3 (x1, y1, z1) (x2, y2, z2) = x1 +. x2, y1 +. y2, z1 +. z2

let cross3 (x1, y1, z1) (x2, y2, z2) =
  (y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2)
;;

let dot3 (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)
let mul3 (x1, y1, z1) c = x1 *. c, y1 *. c, z1 *. c
let div3 (x1, y1, z1) c = x1 /. c, y1 /. c, z1 /. c

type vec2 = float * float [@@deriving show, equal]

let intersect2 p r q s =
  let t = cross2 (sub2 q p) s /. cross2 r s in
  let u = cross2 (sub2 p q) r /. cross2 s r in
  if equal_float (cross2 r s) 0.
  then
    if equal_float (cross2 (sub2 q p) r) 0.
    then
      Format.sprintf
        "collinear: %s %s %s %s"
        (show_vec2 p)
        (show_vec2 r)
        (show_vec2 q)
        (show_vec2 s)
      |> failwith
    else None
  else if compare_float 0. t <= 0 && compare_float 0. u <= 0
  then (* Stdio.printf "%f %f\n" t u; *)
    Some (add2 p (mul2 r t))
  else None
;;

let count_intersections2 minb maxb hailstones =
  let count2 =
    List.cartesian_product hailstones hailstones
    |> List.filter ~f:(fun ((p1, v1), (p2, v2)) ->
      not (equal_vec2 p1 p2 && equal_vec2 v1 v2))
    |> List.count ~f:(fun ((p1, v1), (p2, v2)) ->
      let intersection = intersect2 p1 v1 p2 v2 in
      match intersection with
      | Some (xi, yi) ->
        (* Stdio.printf *)
        (*   "intersection %s %s %s %s @ %f, %f\n" *)
        (*   (show_vec2 p1) *)
        (*   (show_vec2 v1) *)
        (*   (show_vec2 p2) *)
        (*   (show_vec2 v2) *)
        (*   xi *)
        (*   yi; *)
        compare_float xi minb = 1
        && compare_float xi maxb = -1
        && compare_float yi minb = 1
        && compare_float yi maxb = -1
      | None -> false)
  in
  count2 / 2
;;

let starting_position hailstones =
  let[@warning "-8"] ((p0, v0) :: (p1, v1) :: (p2, v2) :: _) = hailstones in
  let p1' = sub3 p1 p0 in
  let p2' = sub3 p2 p0 in
  let v1' = sub3 v1 v0 in
  let v2' = sub3 v2 v0 in
  let t1 = round (-.(cross3 p1' p2' |> dot3 v2') /. (cross3 v1' p2' |> dot3 v2')) in
  let t2 = round (-.(cross3 p1' p2' |> dot3 v1') /. (cross3 p1' v2' |> dot3 v1')) in
  let c1 = add3 p1 (mul3 v1 t1) in
  let c2 = add3 p2 (mul3 v2 t2) in
  let v = div3 (sub3 c2 c1) (t2 -. t1) in
  let x, y, z = sub3 c1 (mul3 v t1) in
  (* Stdio.printf "t1=%f t2=%f\n" t1 t2; *)
  (* Stdio.printf "x=%f y=%f z=%f\n" x y z; *)
  x +. y +. z
;;

let () =
  let ic = Stdio.In_channel.create "day24.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_hail = parse test_input in
  let hail = parse input in
  vec3to2 test_hail |> count_intersections2 7. 27. |> Stdio.printf "Test 1: %d\n%!";
  vec3to2 hail
  |> count_intersections2 200000000000000. 400000000000000.
  |> Stdio.printf "Output 1: %d\n%!";
  starting_position test_hail |> Stdio.printf "Test 2: %f\n%!";
  starting_position hail |> Stdio.printf "Output 2: %f\n%!"
;;
