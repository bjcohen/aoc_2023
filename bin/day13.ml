open! Core

let test_input =
  {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#|}
;;

let parse input =
  let boards, rest =
    String.split_lines input
    |> List.map ~f:String.to_list
    |> List.fold_left ~init:([], []) ~f:(fun (outer, inner) l ->
      if List.is_empty l then List.rev inner :: outer, [] else outer, l :: inner)
  in
  List.rev (List.rev rest :: boards)
;;

type reflection_line =
  | Vertical of int
  | Horizontal of int

let find_reflection' n_diffs board =
  List.init (List.length board - 1) ~f:succ
  |> List.find ~f:(fun y ->
    n_diffs
    = (List.init (Int.min y (List.length board - y)) ~f:succ
       |> List.fold ~init:0 ~f:(fun acc i ->
         acc
         + (List.zip_exn (List.nth_exn board (y - i)) (List.nth_exn board (y + i - 1))
            |> List.count ~f:(fun (x, y) -> not (equal_char x y))))))
;;

let find_reflection n_diffs board =
  match find_reflection' n_diffs board with
  | Some r -> Horizontal r
  | None ->
    (match List.transpose_exn board |> find_reflection' n_diffs with
     | Some r -> Vertical r
     | None -> failwith "no reflection found")
;;

let find_all_reflections n_diffs boards =
  List.map boards ~f:(fun b ->
    match find_reflection n_diffs b with
    | Vertical x -> x
    | Horizontal x -> 100 * x)
  |> List.fold ~init:0 ~f:( + )
;;

let () =
  let test_boards = parse test_input in
  let ic = Stdio.In_channel.create "day13.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let boards = parse input in
  find_all_reflections 0 test_boards |> Stdio.printf "Test 1: %d\n%!";
  find_all_reflections 0 boards |> Stdio.printf "Output 1: %d\n%!";
  find_all_reflections 1 test_boards |> Stdio.printf "Test 2: %d\n%!";
  find_all_reflections 1 boards |> Stdio.printf "Output 2: %d\n%!"
;;
