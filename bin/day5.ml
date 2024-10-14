let test_input_5 =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}
;;

type map = (int * int * int) list

let parse : string -> int list * map list =
  fun input ->
  let lines = String.split_on_char '\n' input in
  let seeds =
    String.sub (List.nth lines 0) 7 (String.length (List.nth lines 0) - 7)
    |> String.split_on_char ' '
    |> List.map int_of_string
  in
  let map_lines = Core.List.drop lines 2 in
  ( seeds
  , List.fold_left
      (fun (maps, map) line ->
        match map, line with
        | None, "" -> failwith "can't happen"
        | None, _ -> maps, Some []
        | Some map', "" -> maps @ [ map' ], None
        | Some map', _ ->
          let vals = String.split_on_char ' ' line in
          ( maps
          , Some
              (map'
               @ [ ( List.nth vals 0 |> int_of_string
                   , List.nth vals 1 |> int_of_string
                   , List.nth vals 2 |> int_of_string )
                 ]) ))
      ([], None)
      map_lines
    |> Core.Tuple2.get1 )
;;

let map_location : map list -> int -> int =
  fun maps seed ->
  List.fold_left
    (fun n map ->
      List.find_map
        (fun (d, s, l) -> if n >= s && n <= s + l then Some (d + n - s) else None)
        map
      |> Option.value ~default:n)
    seed
    maps
;;

let map_range : map -> int * int -> (int * int) list =
  fun map (start, len) ->
  let map_sorted = List.sort (fun (_, s, _) (_, s', _) -> s - s') map in
  let _, last_s, last_l = Core.List.last_exn map_sorted in
  List.concat
    [ List.fold_left
        (fun (acc, last) (d, s, l) ->
          let before = if last < s then [ max start last, s - max start last ] else [] in
          if start < s && start + len - 1 < s
          then acc @ before, s + l
          else if start < s && start + len - 1 < s + l - 1
          then acc @ before @ [ d, start + len - s + 1 ], s + l
          else if start < s && start + len - 1 >= s + l - 1
          then acc @ before @ [ d, l ], s + l
          else if start >= s && start + len - 1 < s + l - 1
          then acc @ [ start - s + d, len ], s + l
          else if start >= s && start < s + l
          then acc @ [ start - s + d, s + l - start ], s + l
          else acc, s + l)
        ([], 0)
        map_sorted
      |> Core.Tuple2.get1
    ; (if start + len > last_s + last_l
       then [ max start (last_s + last_l), start + len - max start (last_s + last_l) ]
       else [])
    ]
;;

let map_all_ranges : int list -> map list -> int =
  fun seeds maps ->
  let seeds_ranges =
    Core.List.groupi seeds ~break:(fun i _ _ -> i mod 2 = 0)
    |> List.map (fun l -> List.nth l 0, List.nth l 1)
  in
  List.fold_left (fun s m -> List.concat_map (map_range m) s) seeds_ranges maps
  |> List.map Core.Tuple2.get1
  |> List.fold_left min Int.max_int
;;

let () =
  let test_seeds, test_maps = parse test_input_5 in
  let ic = open_in "day5.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let seeds, maps = parse input in
  List.map (map_location maps) seeds
  |> List.fold_left min Int.max_int
  |> Format.printf "Test 1: %d\n";
  List.map (map_location maps) seeds
  |> List.fold_left min Int.max_int
  |> Format.printf "Output 1: %d\n";
  map_all_ranges test_seeds test_maps |> Format.printf "Test 2: %d\n";
  map_all_ranges seeds maps |> Format.printf "Output 2: %d\n"
;;
