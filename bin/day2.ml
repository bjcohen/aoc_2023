let test_input_1 : string =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
;;

type game = int * (int * int * int) list [@@deriving show]

let parse : string -> game list =
  fun input ->
  let lines = String.split_on_char '\n' input in
  let parse_game : string -> game =
    fun line ->
    let parts = String.split_on_char ':' line in
    let id, cubespecs =
      match parts with
      | [ id; cubespecs ] -> id, cubespecs
      | _ -> failwith (Format.sprintf "bad input: %s" line)
    in
    let i = Stdlib.int_of_string (List.nth (String.split_on_char ' ' id) 1) in
    let cubespecs' = String.split_on_char ';' cubespecs in
    let parse_cubespec : string -> int * int * int =
      fun cubespec ->
      ( (if Str.string_match (Str.regexp ".* \\([0-9]+\\) red") cubespec 0
         then Str.matched_group 1 cubespec |> Stdlib.int_of_string
         else 0)
      , (if Str.string_match (Str.regexp ".* \\([0-9]+\\) green") cubespec 0
         then Str.matched_group 1 cubespec |> Stdlib.int_of_string
         else 0)
      , if Str.string_match (Str.regexp ".* \\([0-9]+\\) blue") cubespec 0
        then Str.matched_group 1 cubespec |> Stdlib.int_of_string
        else 0 )
    in
    i, List.map parse_cubespec cubespecs'
  in
  List.filter (fun line -> line <> "") lines |> List.map parse_game
;;

let find_possible_games : game list -> int =
  fun games ->
  List.filter_map
    (fun g ->
      let i, cubespecs = g in
      if List.fold_left
           ( && )
           true
           (List.map (fun (r, g, b) -> r <= 12 && g <= 13 && b <= 14) cubespecs)
      then Some i
      else None)
    games
  |> List.fold_left ( + ) 0
;;

let find_power_min_games : game list -> int =
  fun games ->
  List.map
    (fun (_, game) ->
      List.fold_left
        (fun (r, g, b) (r', g', b') -> Int.max r r', Int.max g g', Int.max b b')
        (Int.min_int, Int.min_int, Int.min_int)
        game)
    games
  |> List.map (fun (r, g, b) -> r * g * b)
  |> List.fold_left ( + ) 0
;;

let () =
  let test_games_1 = parse test_input_1 in
  find_possible_games test_games_1 |> Format.printf "Test 1: %d\n";
  let ic = open_in "day2.txt" in
  let games_1 = In_channel.input_all ic |> parse in
  close_in ic;
  find_possible_games games_1 |> Format.printf "Output 1: %d\n";
  find_power_min_games test_games_1 |> Format.printf "Test 2: %d\n";
  find_power_min_games games_1 |> Format.printf "Output 2: %d\n"
;;
