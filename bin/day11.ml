let test_input =
  {|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....|}
;;

let parse input =
  let rows = String.split_on_char '\n' input in
  let galaxies =
    List.mapi
      (fun y row ->
        List.mapi
          (fun x c -> if c = '#' then Some (x, y) else None)
          (String.to_seq row |> List.of_seq))
      rows
    |> List.flatten
    |> List.filter_map Core.Fn.id
  in
  let w = String.length (List.hd rows) in
  let h = List.length rows in
  let x_expansions =
    Seq.ints 0
    |> Seq.take w
    |> Seq.filter (fun i -> not (List.exists (fun (x, _) -> x = i) galaxies))
    |> List.of_seq
  in
  let y_expansions =
    Seq.ints 0
    |> Seq.take h
    |> Seq.filter (fun j -> not (List.exists (fun (_, y) -> y = j) galaxies))
    |> List.of_seq
  in
  galaxies, x_expansions, y_expansions
;;

type galaxies = (int * int) list [@@deriving show]
type expansions = int list [@@deriving show]

let sum_shortest_paths (galaxies, x_expansions, y_expansions) n =
  Core.List.cartesian_product galaxies galaxies
  |> List.map (fun ((x1, y1), (x2, y2)) ->
    abs (x1 - x2)
    + ((n - 1) * Core.List.count ~f:(fun x -> x > min x1 x2 && x < max x1 x2) x_expansions)
    + abs (y1 - y2)
    + ((n - 1) * Core.List.count ~f:(fun y -> y > min y1 y2 && y < max y1 y2) y_expansions))
  |> List.fold_left ( + ) 0
  |> Core.Fn.flip ( / ) 2
;;

let () =
  let test_galaxy = parse test_input in
  let ic = open_in "day11.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let galaxy = parse input in
  sum_shortest_paths test_galaxy 1 |> Format.printf "Test 1: %d\n";
  sum_shortest_paths galaxy 1 |> Format.printf "Output 1: %d\n";
  sum_shortest_paths test_galaxy 10 |> Format.printf "Test 2a: %d\n";
  sum_shortest_paths test_galaxy 100 |> Format.printf "Test 2b: %d\n";
  sum_shortest_paths galaxy 1000000 |> Format.printf "Output 2: %d\n"
;;
