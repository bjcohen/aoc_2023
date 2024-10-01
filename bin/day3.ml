let test_input_1 : string =
  {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
;;

let is_symbol : char -> bool =
  fun c ->
  match c with
  | '0' .. '9' -> false
  | '.' -> false
  | _ -> true
;;

let is_gear : char -> bool = ( = ) '*'

let find_adjacent_symbol : (char -> bool) -> int -> int -> string list -> (int * int) list
  =
  fun symbol_pred i j lines ->
  let width = String.length (List.nth lines 0) in
  let height = List.length lines in
  [ (if i > 0 && j > 0 && symbol_pred (String.get (List.nth lines (j - 1)) (i - 1))
     then Some (i - 1, j - 1)
     else None)
  ; (if j > 0 && symbol_pred (String.get (List.nth lines (j - 1)) i)
     then Some (i, j - 1)
     else None)
  ; (if i < width - 1
        && j > 0
        && symbol_pred (String.get (List.nth lines (j - 1)) (i + 1))
     then Some (i + 1, j - 1)
     else None)
  ; (if i < width - 1 && symbol_pred (String.get (List.nth lines j) (i + 1))
     then Some (i + 1, j)
     else None)
  ; (if i < width - 1
        && j < height - 1
        && symbol_pred (String.get (List.nth lines (j + 1)) (i + 1))
     then Some (i + 1, j + 1)
     else None)
  ; (if j < height - 1 && symbol_pred (String.get (List.nth lines (j + 1)) i)
     then Some (i, j + 1)
     else None)
  ; (if i > 0
        && j < height - 1
        && symbol_pred (String.get (List.nth lines (j + 1)) (i - 1))
     then Some (i - 1, j + 1)
     else None)
  ; (if i > 0 && symbol_pred (String.get (List.nth lines j) (i - 1))
     then Some (i - 1, j)
     else None)
  ]
  |> List.filter_map (fun x -> x)
;;

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y
;;

let find_part_nums : string -> int list =
  fun input ->
  let input = String.trim input in
  let lines = String.split_on_char '\n' input in
  let width = String.length (List.nth lines 0) + 1 in
  let acc, l =
    String.to_seqi (input ^ "\n")
    |> Seq.fold_left
         (fun (acc, l) (i, c) ->
           match acc, c with
           | Some (acc, is_part_num), '0' .. '9' ->
             ( Some
                 ( (acc * 10) + Char.code c - Char.code '0'
                 , is_part_num
                   || List.length
                        (find_adjacent_symbol
                           is_symbol
                           (modulo i width)
                           (i / width)
                           lines)
                      > 0 )
             , l )
           | Some (acc, is_part_num), _ -> None, if is_part_num then l @ [ acc ] else l
           | None, '0' .. '9' ->
             ( Some
                 ( Char.code c - Char.code '0'
                 , List.length
                     (find_adjacent_symbol is_symbol (modulo i width) (i / width) lines)
                   > 0 )
             , l )
           | None, _ -> None, l)
         (None, [])
  in
  let () = assert (acc = None) in
  l
;;

module IntPair = struct
  open Core

  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let find_gear_ratios : string -> int list =
  fun input ->
  let input = String.trim input in
  let lines = String.split_on_char '\n' input in
  let width = String.length (List.nth lines 0) + 1 in
  let acc, gears =
    String.to_seqi (input ^ "\n")
    |> Seq.fold_left
         (fun (acc, m) (i, c) ->
           match acc, c with
           | Some (acc', gear_coords), '0' .. '9' ->
             ( Some
                 ( (acc' * 10) + Char.code c - Char.code '0'
                 , find_adjacent_symbol is_gear (modulo i width) (i / width) lines
                   |> List.fold_left Base.Set.add gear_coords )
             , m )
           | Some (acc', gear_coords), _ ->
             ( None
             , Base.Set.fold gear_coords ~init:m ~f:(fun m' gear_coord ->
                 Base.Map.update m' gear_coord ~f:(fun v ->
                   match v with
                   | Some v' -> v' @ [ acc' ]
                   | None -> [ acc' ])) )
           | None, '0' .. '9' ->
             ( Some
                 ( Char.code c - Char.code '0'
                 , find_adjacent_symbol is_gear (modulo i width) (i / width) lines
                   |> List.fold_left Base.Set.add (Base.Set.empty (module IntPair)) )
             , m )
           | None, _ -> None, m)
         (None, Base.Map.empty (module IntPair))
  in
  assert (acc = None);
  Base.Map.fold gears ~init:[] ~f:(fun ~key:_ ~data:nums acc ->
    match nums with
    | [ num1; num2 ] -> acc @ [ num1 * num2 ]
    | _ -> acc)
;;

let () =
  find_part_nums test_input_1 |> List.fold_left ( + ) 0 |> Format.printf "Test 1: %d\n";
  let ic = open_in "day3.txt" in
  let input = In_channel.input_all ic in
  close_in ic;
  find_part_nums input |> List.fold_left ( + ) 0 |> Format.printf "Output 1: %d\n";
  find_gear_ratios test_input_1 |> List.fold_left ( + ) 0 |> Format.printf "Test 2: %d\n";
  find_gear_ratios input |> List.fold_left ( + ) 0 |> Format.printf "Output 2: %d\n"
;;
