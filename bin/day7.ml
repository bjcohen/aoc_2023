let test_input = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let parse : string -> (string * int) list =
  fun input ->
  String.split_on_char '\n' input
  |> List.filter_map (fun line ->
    if line <> "" then Some (Scanf.sscanf line "%s %d" (fun s n -> s, n)) else None)
;;

let card_to_int : bool -> char -> int =
  fun jokers c ->
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> if jokers then 1 else 11
  | 'T' -> 10
  | '2' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> failwith "unhandled card type"
;;

let compare_card : (char -> int) -> char -> char -> int =
  fun fn c1 c2 -> compare (fn c1) (fn c2)
;;

let total_winnings : bool -> (string * int) list -> int =
  fun jokers hands ->
  let hands_sorted =
    List.sort
      (fun (h1, _) (h2, _) ->
        let groups1 =
          Core.String.to_list h1
          |> List.filter (fun c -> c <> 'J' || not jokers)
          |> List.sort Char.compare
          |> Core.List.group ~break:( <> )
          |> List.sort (fun l1 l2 -> -Int.compare (List.length l1) (List.length l2))
        in
        let num_jokers1 =
          List.filter (( = ) 'J') (Core.String.to_list h1) |> List.length
        in
        let counts1 =
          if num_jokers1 = 5
          then [ 5 ]
          else
            List.mapi
              (fun i g ->
                if i = 0 && jokers then num_jokers1 + List.length g else List.length g)
              groups1
        in
        let groups2 =
          Core.String.to_list h2
          |> List.filter (fun c -> c <> 'J' || not jokers)
          |> List.sort Char.compare
          |> Core.List.group ~break:( <> )
          |> List.sort (fun l1 l2 -> -Int.compare (List.length l1) (List.length l2))
        in
        let num_jokers2 =
          List.filter (( = ) 'J') (Core.String.to_list h2) |> List.length
        in
        let counts2 =
          if num_jokers2 = 5
          then [ 5 ]
          else
            List.mapi
              (fun i g ->
                if i = 0 && jokers then num_jokers2 + List.length g else List.length g)
              groups2
        in
        if List.hd counts1 <> List.hd counts2
        then Int.compare (List.hd counts1) (List.hd counts2)
        else if List.length counts1 > 1 && List.hd (List.tl counts1) <> List.hd (List.tl counts2)
        then Int.compare (List.hd (List.tl counts1)) (List.hd (List.tl counts2))
        else
          List.fold_left2
            (fun acc c1 c2 -> (100 * acc) + compare_card (card_to_int jokers) c1 c2)
            0
            (Core.String.to_list h1)
            (Core.String.to_list h2))
      hands
  in
  List.mapi (fun i (_, bid) -> (i + 1) * bid) hands_sorted |> List.fold_left ( + ) 0
;;

let () =
  let test_hands = parse test_input in
  let ic = open_in "day7.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let hands = parse input in
  total_winnings false test_hands |> Format.printf "Test 1: %d\n";
  total_winnings false hands |> Format.printf "Output 1: %d\n";
  total_winnings true test_hands |> Format.printf "Test 2: %d\n";
  total_winnings true hands |> Format.printf "Output 2: %d\n"
;;
