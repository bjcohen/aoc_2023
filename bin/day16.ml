open! Core

let test_input =
  {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|}
;;

let parse input =
  String.strip input |> String.split ~on:'\n' |> List.map ~f:String.to_list
;;

type energized =
  ((bool * bool * bool * bool)
  [@printer
    fun fmt (u, r, d, l) ->
      fprintf
        fmt
        "%s%s%s%s"
        (if u then "u" else "_")
        (if r then "r" else "_")
        (if d then "d" else "_")
        (if l then "l" else "_")])
    list
    list
[@@deriving show]

type beams = (int * int * char) list [@@deriving show]

let rec find_energized' grid (energized : energized) (beams : beams) =
  if List.is_empty beams
  then energized
  else (
    let energized' =
      List.mapi energized ~f:(fun y row ->
        List.mapi row ~f:(fun x (u, r, d, l) ->
          let dirs =
            List.filter_map beams ~f:(fun (x', y', dir) ->
              if x = x' && y = y' then Some dir else None)
          in
          ( u || List.exists dirs ~f:(fun dir -> equal_char dir 'u')
          , r || List.exists dirs ~f:(fun dir -> equal_char dir 'r')
          , d || List.exists dirs ~f:(fun dir -> equal_char dir 'd')
          , l || List.exists dirs ~f:(fun dir -> equal_char dir 'l') )))
    in
    let beams' =
      List.fold beams ~init:[] ~f:(fun acc (x, y, dir) ->
        let cell = List.nth_exn (List.nth_exn grid y) x in
        (* Stdio.printf "%c %c\n" cell dir; *)
        let beams' =
          match cell, dir with
          | '.', 'u' -> [ x, y - 1, 'u' ]
          | '.', 'r' -> [ x + 1, y, 'r' ]
          | '.', 'd' -> [ x, y + 1, 'd' ]
          | '.', 'l' -> [ x - 1, y, 'l' ]
          | '|', 'u' -> [ x, y - 1, 'u' ]
          | '|', 'r' -> [ x, y - 1, 'u'; x, y + 1, 'd' ]
          | '|', 'd' -> [ x, y + 1, 'd' ]
          | '|', 'l' -> [ x, y - 1, 'u'; x, y + 1, 'd' ]
          | '-', 'u' -> [ x - 1, y, 'l'; x + 1, y, 'r' ]
          | '-', 'r' -> [ x + 1, y, 'r' ]
          | '-', 'd' -> [ x - 1, y, 'l'; x + 1, y, 'r' ]
          | '-', 'l' -> [ x - 1, y, 'l' ]
          | '\\', 'u' -> [ x - 1, y, 'l' ]
          | '\\', 'r' -> [ x, y + 1, 'd' ]
          | '\\', 'd' -> [ x + 1, y, 'r' ]
          | '\\', 'l' -> [ x, y - 1, 'u' ]
          | '/', 'u' -> [ x + 1, y, 'r' ]
          | '/', 'r' -> [ x, y - 1, 'u' ]
          | '/', 'd' -> [ x - 1, y, 'l' ]
          | '/', 'l' -> [ x, y + 1, 'd' ]
          | _ -> Format.sprintf "unhandled %d,%d %c %c" x y dir cell |> failwith
        in
        (* Stdio.printf "%s\n" (show_beams beams'); *)
        acc
        @ List.filter beams' ~f:(fun (x, y, dir) ->
          x >= 0
          && y >= 0
          && x < List.length (List.nth_exn grid 0)
          && y < List.length grid
          &&
          let u, r, d, l = List.nth_exn (List.nth_exn energized y) x in
          match dir with
          | 'u' -> not u
          | 'r' -> not r
          | 'd' -> not d
          | 'l' -> not l
          | _ -> Format.sprintf "unhandled direction %c" dir |> failwith))
    in
    (* Stdio.printf "%s\n" (show_energized energized'); *)
    (* Stdio.printf "%s\n" (show_beams beams'); *)
    find_energized' grid energized' beams')
;;

let find_energized grid =
  find_energized'
    grid
    (List.map grid ~f:(fun row -> List.map row ~f:(fun _ -> false, false, false, false)))
    [ 0, 0, 'r' ]
;;

let count_energized energized =
  List.fold energized ~init:0 ~f:(fun acc row ->
    acc + List.count row ~f:(fun (u, r, d, l) -> u || r || d || l))
;;

let find_max_energized grid =
  let nx = List.length (List.nth_exn grid 0) in
  let ny = List.length grid in
  List.map
    ((List.init nx ~f:succ |> List.map ~f:(fun x -> x - 1, 0, 'd'))
     @ (List.init ny ~f:succ |> List.map ~f:(fun y -> 0, y - 1, 'r'))
     @ (List.init nx ~f:succ |> List.map ~f:(fun x -> x - 1, ny - 1, 'u'))
     @ (List.init ny ~f:succ |> List.map ~f:(fun y -> nx - 1, y - 1, 'l')))
    ~f:(fun beam ->
      Stdio.printf "%s\n%!" (show_beams [ beam ]);
      find_energized'
        grid
        (List.map grid ~f:(fun row ->
           List.map row ~f:(fun _ -> false, false, false, false)))
        [ beam ]
      |> count_energized)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let () =
  let test_grid = parse test_input in
  let ic = Stdio.In_channel.create "day16.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let grid = parse input in
  find_energized test_grid |> count_energized |> Stdio.printf "Test 1: %d\n%!";
  find_energized grid |> count_energized |> Stdio.printf "Output 1: %d\n%!";
  find_max_energized test_grid |> Stdio.printf "Test 2: %d\n%!";
  find_max_energized grid |> Stdio.printf "Output 2: %d\n%!"
;;
