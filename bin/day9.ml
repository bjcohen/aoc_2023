let test_input = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|}

let parse input =
  String.split_on_char '\n' input
  |> List.filter (( <> ) "")
  |> List.map (fun line -> String.split_on_char ' ' line |> List.map int_of_string)
;;

let rec extrapolate_values values =
  let first = List.hd values in
  if List.for_all (( = ) first) values
  then first
  else (
    let _, diffs =
      List.fold_left
        (fun (last, l) x ->
          match last with
          | Some y -> Some x, l @ [ x - y ]
          | None -> Some x, l)
        (None, [])
        values
    in
    Core.List.last_exn values + extrapolate_values diffs)
;;

let rec extrapolate_values_backward values =
  let first = List.hd values in
  if List.for_all (( = ) first) values
  then first
  else (
    let _, diffs =
      List.fold_left
        (fun (last, l) x ->
          match last with
          | Some y -> Some x, l @ [ x - y ]
          | None -> Some x, l)
        (None, [])
        values
    in
    first - extrapolate_values_backward diffs)
;;

let () =
  let test_values = parse test_input in
  let ic = open_in "day9.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  let values = parse input in
  List.map extrapolate_values test_values
  |> List.fold_left ( + ) 0
  |> Format.printf "Test 1: %d\n";
  List.map extrapolate_values values
  |> List.fold_left ( + ) 0
  |> Format.printf "Output 1: %d\n";
  List.map extrapolate_values_backward test_values
  |> List.fold_left ( + ) 0
  |> Format.printf "Test 2: %d\n";
  List.map extrapolate_values_backward values
  |> List.fold_left ( + ) 0
  |> Format.printf "Output 2: %d\n"
;;
