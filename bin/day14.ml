open! Core

let test_input =
  {|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....|}
;;

let parse input =
  String.split input ~on:'\n'
  |> List.filter_map ~f:(fun l ->
    if String.is_empty l then None else Some (String.to_list l))
  |> List.transpose_exn
;;

let tilt' rev platform =
  List.map
    ~f:(fun col ->
      List.group col ~break:(fun c1 c2 ->
        (equal_char c1 '#' || equal_char c2 '#') && not (equal_char c1 c2))
      |> List.concat_map ~f:(fun l ->
        List.sort
          ~compare:((if not rev then Fn.id else Comparable.reverse) Char.compare)
          l))
    platform
;;

let tilt rev = Memo.general (tilt' rev)

let load platform =
  List.map
    ~f:(fun col ->
      List.mapi ~f:(fun y c -> if equal_char c 'O' then List.length col - y else 0) col
      |> List.fold ~init:0 ~f:( + ))
    platform
  |> List.fold ~init:0 ~f:( + )
;;

module Platform = struct
  type t = char list list [@@deriving hash, compare, sexp]
end

let rec rotate_tilt platform i tbl =
  if i = 1_000_000_000
  then platform
  else (
    let platform' =
      platform
      |> tilt true
      |> List.transpose_exn
      |> tilt true
      |> List.transpose_exn
      |> tilt false
      |> List.transpose_exn
      |> tilt false
      |> List.transpose_exn
    in
    match Hashtbl.add tbl ~key:platform ~data:i with
    | `Duplicate ->
      let prev_i = Hashtbl.find_exn tbl platform in
      rotate_tilt platform' (i + 1 + ((1_000_000_000 - i) / (i - prev_i) * (i - prev_i))) tbl
    | `Ok -> rotate_tilt platform' (i + 1) tbl)
;;

let () =
  let test_platform = parse test_input in
  let ic = Stdio.In_channel.create "day14.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let platform = parse input in
  tilt true test_platform |> load |> Stdio.printf "Test 1: %d\n%!";
  tilt true platform |> load |> Stdio.printf "Output 1: %d\n%!";
  rotate_tilt test_platform 0 (Hashtbl.create (module Platform))
  |> load
  |> Stdio.printf "Test 2: %d\n%!";
  rotate_tilt platform 0 (Hashtbl.create (module Platform))
  |> load
  |> Stdio.printf "Output 2: %d\n%!"
;;
