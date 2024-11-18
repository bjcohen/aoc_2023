open! Core

let test_input =
  {|R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)|}
;;

let parse1 input =
  String.strip input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun l ->
    let parts = String.split l ~on:' ' in
    ( List.nth_exn parts 0 |> String.to_list |> List.hd_exn
    , List.nth_exn parts 1 |> int_of_string ))
;;

let parse2 input =
  String.strip input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun l ->
    let parts = String.split l ~on:' ' in
    ( (match List.nth_exn parts 2 |> (Fn.flip String.get) 7 with
       | '0' -> 'R'
       | '1' -> 'D'
       | '2' -> 'L'
       | '3' -> 'U'
       | _ as d -> Format.sprintf "unhandled direction [%c]" d |> failwith)
    , List.nth_exn parts 2 |> String.sub ~pos:2 ~len:5 |> fun x -> int_of_string ("0x" ^ x)
    ))
;;

let plan_to_coords plan =
  List.fold
    plan
    ~init:[ 0, 0 ]
    ~f:(fun cs (d, n) ->
      let lx, ly = List.last_exn cs in
      cs
      @ [ (match d with
           | 'U' -> lx, ly - n
           | 'R' -> lx + n, ly
           | 'D' -> lx, ly + n
           | 'L' -> lx - n, ly
           | _ -> Format.sprintf "unhandled direction [%c]" d |> failwith)
        ])
;;

type plan_coords = (int * int) list [@@deriving show]

let area_of_coords plan_coords =
  (* Stdio.printf "%s\n%!" (show_plan_coords plan_coords); *)
  List.fold2_exn
    plan_coords
    (List.tl_exn plan_coords @ [ List.hd_exn plan_coords ])
    ~init:0
    ~f:(fun acc (x1, y1) (x2, y2) -> acc + (x1 * y2) - (x2 * y1))
  |> Fn.flip ( / ) 2
;;

let area_of_border plan = 1 + (List.fold plan ~init:0 ~f:(fun acc (_, n) -> acc + n) / 2)
let area_of_plan plan = (plan_to_coords plan |> area_of_coords) + area_of_border plan

let () =
  let ic = Stdio.In_channel.create "day18.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  area_of_plan (parse1 test_input) |> Stdio.printf "Test 1: %d\n%!";
  area_of_plan (parse1 input) |> Stdio.printf "Output 1: %d\n%!";
  area_of_plan (parse2 test_input) |> Stdio.printf "Test 1: %d\n%!";
  area_of_plan (parse2 input) |> Stdio.printf "Output 1: %d\n%!"
;;
