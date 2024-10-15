let test_input =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}
;;

let test_input2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let test_input3 =
  {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|}
;;

let parse : string -> char Seq.t * (string * (string * string)) list =
  fun input ->
  let lines = String.trim input |> String.split_on_char '\n' in
  let directions = List.hd lines |> String.to_seq in
  let nodes =
    List.tl (List.tl lines)
    |> List.map (fun l ->
      Scanf.sscanf l "%[A-Z0-9] = (%[A-Z0-9], %[A-Z0-9])" (fun a b c -> a, (b, c)))
  in
  directions, nodes
;;

let rec follow_directions'
  :  char Seq.t -> (string * (string * string)) list -> int -> (string -> bool) -> string
  -> int
  =
  fun directions nodes i stop curr_node ->
  if stop curr_node
  then i
  else (
    let l, r = List.assoc curr_node nodes in
    match Seq.uncons directions with
    | Some (lr, rest) ->
      follow_directions' rest nodes (i + 1) stop (if lr = 'L' then l else r)
    | None -> failwith "can't happen")
;;

let follow_directions : char Seq.t -> (string * (string * string)) list -> int =
  fun directions nodes ->
  follow_directions' (Seq.cycle directions) nodes 0 (( = ) "ZZZ") "AAA"
;;

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b

let follow_directions2 : char Seq.t -> (string * (string * string)) list -> int =
  fun directions nodes ->
  List.filter_map (fun (s, _) -> if String.get s 2 = 'A' then Some s else None) nodes
  |> List.map
       (follow_directions' (Seq.cycle directions) nodes 0 (fun s -> String.get s 2 = 'Z'))
  |> List.fold_left lcm 1
;;

let () =
  let test_directions, test_nodes = parse test_input in
  let test_directions2, test_nodes2 = parse test_input2 in
  let test_directions3, test_nodes3 = parse test_input3 in
  let ic = open_in "day8.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let directions, nodes = parse input in
  follow_directions test_directions test_nodes |> Format.printf "Test 1: %d\n";
  follow_directions test_directions2 test_nodes2 |> Format.printf "Test 1b: %d\n";
  follow_directions directions nodes |> Format.printf "Output 1: %d\n";
  follow_directions2 test_directions3 test_nodes3 |> Format.printf "Test 2: %d\n";
  follow_directions2 directions nodes |> Format.printf "Output 2: %d\n"
;;
