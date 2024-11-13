let test_input = {|-L|F7
7S-7|
L|7||
-L-J|
L|-JF|}

let test_input2 = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...|}

let test_input3 =
  {|FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L|}
;;

type pair = int * int [@@deriving show]
type pairlist = (int * int) list [@@deriving show]

let parse input = String.split_on_char '\n' input

let find_start grid =
  List.find_mapi
    (fun y row ->
      String.to_seq row
      |> Seq.find_mapi (fun x c -> if c = 'S' then Some (x, y) else None))
    grid
  |> Option.get
;;

let lookup grid x y = List.nth grid y |> Core.Fn.flip String.get x
let width grid = List.nth grid 0 |> String.length
let height grid = List.length grid

let can_move_x test grid x y =
  let c = lookup grid x y in
  List.exists (( = ) c) test
;;

let can_move_r = can_move_x [ 'S'; '-'; 'L'; 'F' ]
let can_move_l = can_move_x [ 'S'; '-'; '7'; 'J' ]
let can_move_u = can_move_x [ 'S'; '|'; 'L'; 'J' ]
let can_move_d = can_move_x [ 'S'; '|'; '7'; 'F' ]

let step grid path (x, y) =
  if x < width grid
     && ((not (List.mem (x + 1, y) path))
         || (List.length path > 1 && List.nth path 0 = (x + 1, y)))
     && can_move_r grid x y
     && can_move_l grid (x + 1) y
  then x + 1, y
  else if y < height grid
          && ((not (List.mem (x, y + 1) path))
              || (List.length path > 1 && List.nth path 0 = (x, y + 1)))
          && can_move_d grid x y
          && can_move_u grid x (y + 1)
  then x, y + 1
  else if x > 0
          && ((not (List.mem (x - 1, y) path))
              || (List.length path > 1 && List.nth path 0 = (x - 1, y)))
          && can_move_l grid x y
          && can_move_r grid (x - 1) y
  then x - 1, y
  else if y > 0
          && ((not (List.mem (x, y - 1) path))
              || (List.length path > 1 && List.nth path 0 = (x, y - 1)))
          && can_move_u grid x y
          && can_move_d grid x (y - 1)
  then x, y - 1
  else Format.sprintf "no valid move from (%d, %d)" x y |> failwith
;;

let rec path' grid start_xy xy acc =
  let next = step grid acc xy in
  if next = start_xy then acc @ [ xy ] else path' grid start_xy next (acc @ [ xy ])
;;

let path grid = path' grid (find_start grid) (find_start grid) []
let furthest grid = path grid |> List.length |> Core.Fn.flip ( / ) 2

type path = (int * int) list [@@deriving show]

(* Shoelace formula + Pick's theorem *)
let interior_points grid =
  let p = path grid in
  let area =
    List.fold_left2
      (fun acc (x1, y1) (x2, y2) -> acc + (x1 * y2) - (y1 * x2))
      0
      p
      (List.tl p @ [ List.hd p ])
    |> Core.Fn.flip ( / ) 2
    |> abs
  in
  area + 1 - (List.length p / 2)
;;

let () =
  let test_grid = parse test_input in
  let test_grid2 = parse test_input2 in
  let test_grid3 = parse test_input3 in
  let ic = open_in "day10.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let grid = parse input in
  furthest test_grid |> Format.printf "Test 1: %d\n";
  furthest test_grid2 |> Format.printf "Test 1_2: %d\n";
  furthest grid |> Format.printf "Ouput 1: %d\n";
  interior_points test_grid |> Format.printf "Test 2: %d\n";
  interior_points test_grid2 |> Format.printf "Test 2_2: %d\n";
  interior_points test_grid3 |> Format.printf "Test 2_3: %d\n";
  interior_points grid |> Format.printf "Output 2: %d\n"
;;
