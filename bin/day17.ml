open! Core

let test_input =
  {|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
|}
;;

let test_input_2 = {|111111111111
999999999991
999999999991
999999999991
999999999991
|}

let parse input =
  String.strip input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun l ->
    String.to_list l |> List.map ~f:(fun x -> int_of_char x - int_of_char '0'))
;;

let get_heat grid x y = List.nth_exn (List.nth_exn grid y) x

module Step = struct
  type t = int * int * char * (int [@compare.ignore]) * int [@@deriving hash, compare, sexp]
end

let rec get_min_heat_loss' grid min_steps max_steps queue visited =
  let w = List.length (List.nth_exn grid 0) in
  let h = List.length grid in
  let x, y, dir, heat_loss, steps = Pairing_heap.pop_exn queue in
  (* Stdio.printf "%d, %d, %c, %d, %d\n" x y dir heat_loss steps; *)
  if x = w - 1 && y = h - 1 && steps >= min_steps
  then heat_loss
  else (
    let steps' =
      match dir with
      | 'u' ->
        [ (if steps >= min_steps && x > 0
           then Some (x - 1, y, 'l', heat_loss + get_heat grid (x - 1) y, 1)
           else None)
        ; (if steps >= min_steps && x < w - 1
           then Some (x + 1, y, 'r', heat_loss + get_heat grid (x + 1) y, 1)
           else None)
        ; (if steps < max_steps && y > 0
           then Some (x, y - 1, 'u', heat_loss + get_heat grid x (y - 1), steps + 1)
           else None)
        ]
      | 'r' ->
        [ (if steps >= min_steps && y > 0
           then Some (x, y - 1, 'u', heat_loss + get_heat grid x (y - 1), 1)
           else None)
        ; (if steps >= min_steps && y < h - 1
           then Some (x, y + 1, 'd', heat_loss + get_heat grid x (y + 1), 1)
           else None)
        ; (if steps < max_steps && x < w - 1
           then Some (x + 1, y, 'r', heat_loss + get_heat grid (x + 1) y, steps + 1)
           else None)
        ]
      | 'd' ->
        [ (if steps >= min_steps && x > 0
           then Some (x - 1, y, 'l', heat_loss + get_heat grid (x - 1) y, 1)
           else None)
        ; (if steps >= min_steps && x < w - 1
           then Some (x + 1, y, 'r', heat_loss + get_heat grid (x + 1) y, 1)
           else None)
        ; (if steps < max_steps && y < h - 1
           then Some (x, y + 1, 'd', heat_loss + get_heat grid x (y + 1), steps + 1)
           else None)
        ]
      | 'l' ->
        [ (if steps >= min_steps && y > 0
           then Some (x, y - 1, 'u', heat_loss + get_heat grid x (y - 1), 1)
           else None)
        ; (if steps >= min_steps && y < h - 1
           then Some (x, y + 1, 'd', heat_loss + get_heat grid x (y + 1), 1)
           else None)
        ; (if steps < max_steps && x > 0
           then Some (x - 1, y, 'l', heat_loss + get_heat grid (x - 1) y, steps + 1)
           else None)
        ]
      | _ -> Format.sprintf "unmatched direction [%c]" dir |> failwith
    in
    let steps'' =
      List.filter_map steps' ~f:(function
        | Some step -> if not (Hash_set.mem visited step) then Some step else None
        | None -> None)
    in
    List.iter steps'' ~f:(Hash_set.add visited);
    List.iter steps'' ~f:(Pairing_heap.add queue);
    get_min_heat_loss' grid min_steps max_steps queue visited)
;;

let get_min_heat_loss grid min_steps max_steps =
  let visited = Hash_set.create (module Step) in
  let s1 = 0, 0, 'd', 0, 0 in
  let s2 = 0, 0, 'r', 0, 0 in
  Hash_set.add visited s1;
  Hash_set.add visited s2;
  let queue =
    Pairing_heap.create
      ~cmp:(fun (_, _, _, hl1, _) (_, _, _, hl2, _) -> compare hl1 hl2)
      ()
  in
  Pairing_heap.add queue s1;
  Pairing_heap.add queue s2;
  get_min_heat_loss' grid min_steps max_steps queue visited
;;

let () =
  let test_grid = parse test_input in
  let ic = Stdio.In_channel.create "day17.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let grid = parse input in
  get_min_heat_loss test_grid 1 3 |> Stdio.printf "Test 1: %d\n%!";
  get_min_heat_loss grid 1 3 |> Stdio.printf "Output 1: %d\n%!";
  get_min_heat_loss test_grid 4 10 |> Stdio.printf "Test 2: %d\n%!";
  get_min_heat_loss (parse test_input_2) 4 10 |> Stdio.printf "Test 2b: %d\n%!";
  get_min_heat_loss grid 4 10 |> Stdio.printf "Output 2: %d\n%!"
;;
