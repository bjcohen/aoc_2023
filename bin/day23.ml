open! Core

let test_input =
  {|#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#|}
;;

let parse input = String.strip input |> String.split ~on:'\n'

module IntPair = struct
  type t = int * int [@@deriving hash, compare, sexp, equal]
end

module IntQuad = struct
  type t = int * int * int * int [@@deriving hash, compare, sexp]
end

let grid_is grid x y cs =
  List.mem cs (String.get (List.nth_exn grid y) x) ~equal:equal_char
;;

type queue = (int * int * int) list [@@deriving show]
type keypoint_dists = ((int * int * int * int) * int) list [@@deriving show]

let paths_and_slopes = [ '.'; 'v'; '^'; '>'; '<' ]

module KeypointExit = struct
  type t = int * int * char [@@deriving hash, compare, sexp]
end

let rec get_keypoint_dists grid queue keypoint_dists allow_uphill exited_keypoints =
  match queue with
  | [] ->
    Stdio.printf "%s\n" (show_keypoint_dists (Hashtbl.to_alist keypoint_dists));
    keypoint_dists
  | (x, y, dist, dir, last_keyx, last_keyy) :: queue' ->
    let w = String.length (List.hd_exn grid) in
    let h = List.length grid in
    if x = w - 2 && y = h - 1
    then (
      Hashtbl.update keypoint_dists (last_keyx, last_keyy, -1, -1) ~f:(fun dist' ->
        max (Option.value dist' ~default:0) dist);
      get_keypoint_dists grid queue' keypoint_dists allow_uphill exited_keypoints)
    else (
      let next =
        List.filter_map
          ~f:Fn.id
          [ (if x > 0
                && grid_is
                     grid
                     (x - 1)
                     y
                     (if allow_uphill then paths_and_slopes else [ '.'; '<' ])
                && not (equal_char dir '>')
             then Some (x - 1, y, dist + 1, '<', last_keyx, last_keyy)
             else None)
          ; (if x < w - 1
                && grid_is
                     grid
                     (x + 1)
                     y
                     (if allow_uphill then paths_and_slopes else [ '.'; '>' ])
                && not (equal_char dir '<')
             then Some (x + 1, y, dist + 1, '>', last_keyx, last_keyy)
             else None)
          ; (if y > 0
                && grid_is
                     grid
                     x
                     (y - 1)
                     (if allow_uphill then paths_and_slopes else [ '.'; '^' ])
                && not (equal_char dir 'v')
             then Some (x, y - 1, dist + 1, '^', last_keyx, last_keyy)
             else None)
          ; (if x < h - 1
                && grid_is
                     grid
                     x
                     (y + 1)
                     (if allow_uphill then paths_and_slopes else [ '.'; 'v' ])
                && not (equal_char dir '^')
             then Some (x, y + 1, dist + 1, 'v', last_keyx, last_keyy)
             else None)
          ]
      in
      let next' =
        if List.length next > 1
        then (
          (* Stdio.printf "%d, %d -> %d, %d = %d\n" last_keyx last_keyy x y dist; *)
          Hashtbl.update keypoint_dists (last_keyx, last_keyy, x, y) ~f:(fun dist' ->
            max (Option.value dist' ~default:0) dist);
          List.filter_map next ~f:(fun (x', y', _dist', dir', _last_keyx', _last_keyy') ->
            if Hash_set.mem exited_keypoints (x', y', dir')
            then None
            else Some (x', y', 1, dir', x, y)))
        else next
      in
      if List.length next > 1
      then
        List.iter next ~f:(fun (x', y', _dist', dir, _last_keyx', _last_keyy') ->
          Hash_set.add exited_keypoints (x', y', dir))
      else ();
      get_keypoint_dists
        grid
        (queue' @ next')
        keypoint_dists
        allow_uphill
        exited_keypoints)
;;

let rec find_longest_path_from_keypoints (x, y) seen keypoints =
  if x = -1 && y = -1
  then Some 0
  else
    Hashtbl.find_exn keypoints (x, y)
    |> List.filter ~f:(fun (x2, y2, _) ->
      not (List.mem seen (x2, y2) ~equal:(Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal)))
    |> List.filter_map ~f:(fun (x2, y2, dist) ->
      find_longest_path_from_keypoints (x2, y2) (seen @ [ x2, y2 ]) keypoints
      |> Option.map ~f:(( + ) dist))
    |> List.max_elt ~compare:Int.compare
;;

let longest_path grid allow_uphill =
  let keypoint_dists =
    get_keypoint_dists
      grid
      [ 1, 0, 0, 'v', 1, 0 ]
      (Hashtbl.create (module IntQuad))
      allow_uphill
      (Hash_set.of_list (module KeypointExit) [ 1, 0, 'v' ])
  in
  Hashtbl.fold
    keypoint_dists
    ~init:(Hashtbl.create (module IntPair))
    ~f:(fun ~key:(x1, y1, x2, y2) ~data:dist acc ->
      Hashtbl.add_multi acc ~key:(x1, y1) ~data:(x2, y2, dist);
      acc)
  |> find_longest_path_from_keypoints (1, 0) []
  |> Option.value_exn
;;

let () =
  let ic = Stdio.In_channel.create "day23.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_grid = parse test_input in
  let grid = parse input in
  longest_path test_grid false |> Stdio.printf "Test 1: %d\n%!";
  longest_path grid false |> Stdio.printf "Output 1: %d\n%!";
  longest_path test_grid true |> Stdio.printf "Test 2: %d\n%!";
  longest_path grid true |> Stdio.printf "Output 2: %d\n%!"
;;
