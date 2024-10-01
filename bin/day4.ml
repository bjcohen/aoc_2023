let test_input_4 : string =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
;;

let parse_int_list : string -> int list =
  fun s ->
  String.trim s
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")
  |> List.map int_of_string
;;

let parse : string -> (int list * int list) list =
  fun input ->
  String.split_on_char '\n' input
  |> List.filter (( <> ) "")
  |> List.map (fun line ->
    match
      String.split_on_char ':' line
      |> (Core.Fn.flip List.nth) 1
      |> String.split_on_char '|'
    with
    | [ winning; nums ] -> parse_int_list winning, parse_int_list nums
    | _ -> failwith "unexpected input")
;;

let compute_matches : int list -> int list -> int =
  fun winning nums -> List.filter (fun n -> List.mem n winning) nums |> List.length
;;

let compute_points_all : (int list * int list) list -> int =
  List.fold_left (fun a (w, n) -> a + (compute_matches w n |> fun x -> 1 lsl (x - 1))) 0
;;

let compute_tickets_all : (int list * int list) list -> int =
  fun tickets ->
  List.fold_left
    (fun (c, l) (w, n) ->
      match l with
      | n_tickets :: rest ->
        let n_matches = compute_matches w n in
        let n_tickets_won = n_tickets * n_matches in
        Format.printf "%d %d\n" n_tickets n_matches;
        ( c + 1 + n_tickets_won
        , List.mapi (fun i x -> if i < n_matches then x + n_tickets else x) (rest @ [ 1 ])
        )
      | _ -> failwith "no elements")
    (0, List.nth tickets 0 |> Core.Tuple2.get1 |> List.map (fun _ -> 1))
    tickets
  |> Core.Tuple2.get1
;;

let () =
  let ic = open_in "day4.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  parse test_input_4 |> compute_points_all |> Format.printf "Test 1: %d\n";
  parse input |> compute_points_all |> Format.printf "Output 1: %d\n";
  parse test_input_4 |> compute_tickets_all |> Format.printf "Test 2: %d\n";
  parse input |> compute_tickets_all |> Format.printf "Output 2: %d\n"
;;
