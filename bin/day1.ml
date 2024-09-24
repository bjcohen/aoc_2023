let test_input_1 : string = {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet|}

let test_input_2 : string =
  {|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen|}
;;

let calibration_value : string -> int =
  fun line ->
  let seq = String.to_seq line in
  let vals =
    Seq.fold_left
      (fun x v ->
        if v >= Stdlib.int_of_char '0' && v <= Stdlib.int_of_char '9'
        then (
          let v' = v - Stdlib.int_of_char '0' in
          match x with
          | None -> Some (v', v')
          | Some (x', _) -> Some (x', v'))
        else x)
      None
      (Seq.map Stdlib.int_of_char seq)
  in
  match vals with
  | Some (x, y) -> (10 * x) + y
  | None -> failwith "no numbers found"
;;

let match_num : string -> int -> int option * int option =
  fun line i ->
  if i == String.length line
  then None, None
  else if String.sub line i 1 = "1"
  then Some 1, Some 1
  else if String.sub line i 1 = "2"
  then Some 1, Some 2
  else if String.sub line i 1 = "3"
  then Some 1, Some 3
  else if String.sub line i 1 = "4"
  then Some 1, Some 4
  else if String.sub line i 1 = "5"
  then Some 1, Some 5
  else if String.sub line i 1 = "6"
  then Some 1, Some 6
  else if String.sub line i 1 = "7"
  then Some 1, Some 7
  else if String.sub line i 1 = "8"
  then Some 1, Some 8
  else if String.sub line i 1 = "9"
  then Some 1, Some 9
  else if i + 3 <= String.length line && String.sub line i 3 = "one"
  then Some 2, Some 1
  else if i + 3 <= String.length line && String.sub line i 3 = "two"
  then Some 2, Some 2
  else if i + 5 <= String.length line && String.sub line i 5 = "three"
  then Some 4, Some 3
  else if i + 4 <= String.length line && String.sub line i 4 = "four"
  then Some 4, Some 4
  else if i + 4 <= String.length line && String.sub line i 4 = "five"
  then Some 3, Some 5
  else if i + 3 <= String.length line && String.sub line i 3 = "six"
  then Some 3, Some 6
  else if i + 5 <= String.length line && String.sub line i 5 = "seven"
  then Some 4, Some 7
  else if i + 5 <= String.length line && String.sub line i 5 = "eight"
  then Some 4, Some 8
  else if i + 4 <= String.length line && String.sub line i 4 = "nine"
  then Some 3, Some 9
  else Some 1, None
;;

let calibration_value_2 : string -> int =
  fun line ->
  let rec v2 i xy =
    match match_num line i with
    | None, None -> xy
    | None, Some _ -> failwith "unreachable"
    | Some x, None -> v2 (i + x) xy
    | Some x, Some y ->
      (match xy with
       | None -> v2 (i + x) (Some (y, y))
       | Some (x', _) -> v2 (i + x) (Some (x', y)))
  in
  match v2 0 None with
  | Some (x, y) -> (10 * x) + y
  | None -> failwith "no numbers found"
;;

let sum_map : string list -> (string -> int) -> int =
  fun l f -> List.fold_left ( + ) 0 (List.map f l)
;;

let () =
  let () =
    Format.printf
      "Test 1: %d\n"
      (sum_map (String.split_on_char '\n' test_input_1) calibration_value)
  in
  let ic = open_in "day1.txt" in
  let () =
    Format.printf "Output 1: %d\n" (sum_map (In_channel.input_lines ic) calibration_value)
  in
  let () =
    Format.printf
      "Test 2: %d\n"
      (sum_map (String.split_on_char '\n' test_input_2) calibration_value_2)
  in
  let ic = open_in "day1.txt" in
  Format.printf "Output 2: %d\n" (sum_map (In_channel.input_lines ic) calibration_value_2)
;;
