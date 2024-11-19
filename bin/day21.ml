open! Core

let test_input =
  {|...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........|}
;;

let parse input = String.strip input |> String.split ~on:'\n'

module IntPair = struct
  type t = int * int [@@deriving hash, compare, sexp]
end

let rec n_within' grid steps i (curr : (IntPair.t, int) Hashtbl.t) =
  if i = steps
  then curr
  else (
    let w = String.length (List.hd_exn grid) in
    let h = List.length grid in
    let next = Hashtbl.copy curr in
    Hashtbl.iteri curr ~f:(fun ~key:(x, y) ~data:curr_dist ->
      Hashtbl.update
        next
        (x - 1, y)
        ~f:(Option.value_map ~default:(curr_dist + 1) ~f:(fun d -> min d (curr_dist + 1)));
      Hashtbl.update
        next
        (x + 1, y)
        ~f:(Option.value_map ~default:(curr_dist + 1) ~f:(fun d -> min d (curr_dist + 1)));
      Hashtbl.update
        next
        (x, y - 1)
        ~f:(Option.value_map ~default:(curr_dist + 1) ~f:(fun d -> min d (curr_dist + 1)));
      Hashtbl.update
        next
        (x, y + 1)
        ~f:(Option.value_map ~default:(curr_dist + 1) ~f:(fun d -> min d (curr_dist + 1))));
    let next' =
      Hashtbl.filter_keys next ~f:(fun (x, y) ->
        x >= 0
        && x < w
        && y >= 0
        && y < h
        && not (equal_char '#' (String.get (List.nth_exn grid y) x)))
    in
    if Hashtbl.equal Int.equal curr next'
    then curr
    else n_within' grid steps (i + 1) next')
;;

let n_within grid steps =
  let sx, sy =
    List.find_mapi_exn grid ~f:(fun y row ->
      String.find_mapi row ~f:(fun x c -> if equal_char c 'S' then Some (x, y) else None))
  in
  n_within' grid steps 0 (Hashtbl.of_alist_exn (module IntPair) [ (sx, sy), 0 ])
  |> Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data acc ->
    if data <= steps && data % 2 = 0 then acc + 1 else acc)
;;

let n_within_infinite grid steps =
  let h = List.length grid in
  (* Stdio.printf "h=%d\n" h; *)
  let sx, sy =
    List.find_mapi_exn grid ~f:(fun y row ->
      String.find_mapi row ~f:(fun x c -> if equal_char c 'S' then Some (x, y) else None))
  in
  let dists =
    n_within' grid steps (h * 2) (Hashtbl.of_alist_exn (module IntPair) [ (sx, sy), 0 ])
  in
  let bound = steps % h in
  (* Stdio.printf "bound=%d h-b-1=%d\n" bound (h - bound - 1); *)
  let even_corners =
    Hashtbl.counti dists ~f:(fun ~key:(x, y) ~data:d ->
      d % 2 = 0
      && d > h - bound - 1
      && Int.abs (x - sx) + Int.abs (y - sy) > h - bound - 1)
  in
  let odd_corners =
    Hashtbl.counti dists ~f:(fun ~key:(x, y) ~data:d ->
      d % 2 = 1 && d > bound && Int.abs (x - sx) + Int.abs (y - sy) > bound)
  in
  let even_full = Hashtbl.count dists ~f:(fun d -> d % 2 = 0) in
  let odd_full = Hashtbl.count dists ~f:(fun d -> d % 2 = 1) in
  let n = (steps - bound) / h in
  (* Stdio.printf "bound=%d n=%d even_c=%d odd_c=%d even_f=%d odd_f=%d\n" bound n even_corners odd_corners even_full odd_full; *)
  if steps % 2 = 1
  then
    ((n + 1) * (n + 1) * odd_full)
    + (n * n * even_full)
    - ((n + 1) * odd_corners)
    + (n * even_corners)
  else
    ((n + 1) * (n + 1) * even_full)
    + (n * n * odd_full)
    - ((n + 1) * even_corners)
    + (n * odd_corners)
;;

let () =
  let ic = Stdio.In_channel.create "day21.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_grid = parse test_input in
  let grid = parse input in
  n_within test_grid 6 |> Stdio.printf "Test 1: %d\n%!";
  n_within grid 64 |> Stdio.printf "Output 1: %d\n%!";
  n_within_infinite test_grid 6 |> Stdio.printf "Test 2a: %d\n%!";
  n_within_infinite test_grid 10 |> Stdio.printf "Test 2b: %d\n%!";
  n_within_infinite test_grid 50 |> Stdio.printf "Test 2c: %d\n%!";
  n_within_infinite test_grid 100 |> Stdio.printf "Test 2d: %d\n%!";
  n_within_infinite test_grid 500 |> Stdio.printf "Test 2e: %d\n%!";
  n_within_infinite test_grid 1000 |> Stdio.printf "Test 2f: %d\n%!";
  n_within_infinite test_grid 5000 |> Stdio.printf "Test 2g: %d\n%!";
  n_within_infinite grid 26501365 |> Stdio.printf "Output 2: %d\n%!"
;;
