let test_input = {|Time:      7  15   30
Distance:  9  40  200|}

type races = (int * int) list [@@deriving show]

let parse : string -> races =
  fun input ->
  let lines = String.split_on_char '\n' input in
  let times_line = List.nth lines 0 in
  let time_length = String.length "Time:" in
  let times =
    String.sub times_line time_length (String.length times_line - time_length)
    |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
    |> List.map int_of_string
  in
  let distances_line = List.nth lines 1 in
  let distance_length = String.length "Distance:" in
  let distances =
    String.sub distances_line distance_length (String.length times_line - distance_length)
    |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
    |> List.map int_of_string
  in
  List.combine times distances
;;

let parse2 : string -> int * int =
  fun input ->
  let lines = String.split_on_char '\n' input in
  let time_buf = Buffer.create (String.length (List.nth lines 0)) in
  String.iter
    (fun c ->
      match c with
      | '0' .. '9' -> Buffer.add_char time_buf c
      | _ -> ())
    (List.nth lines 0);
  let dist_buf = Buffer.create (String.length (List.nth lines 1)) in
  String.iter
    (fun c ->
      match c with
      | '0' .. '9' -> Buffer.add_char dist_buf c
      | _ -> ())
    (List.nth lines 1);
  int_of_string (Buffer.contents time_buf), int_of_string (Buffer.contents dist_buf)
;;

let ways_to_beat : int * int -> int =
  fun (time, dist) ->
  Seq.take (time + 1) (Seq.ints 0)
  |> Seq.map (fun i -> i * (time - i))
  |> Seq.filter (fun i -> i > dist)
  |> Seq.length
;;

let () =
  let test_races = parse test_input in
  let ic = open_in "day6.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let races = parse input in
  List.map ways_to_beat test_races
  |> List.fold_left ( * ) 1
  |> Format.printf "Test 1: %d\n";
  List.map ways_to_beat races |> List.fold_left ( * ) 1 |> Format.printf "Output 1: %d\n";
  parse2 test_input |> ways_to_beat |> Format.printf "Test 2: %d\n";
  parse2 input |> ways_to_beat |> Format.printf "Output 2: %d\n"
;;
