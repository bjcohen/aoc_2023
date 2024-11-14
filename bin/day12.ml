open! Base

let test_input =
  {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1|}
;;

let parse input =
  String.split_on_chars ~on:[ '\n' ] input
  |> List.filter ~f:(Core.Fn.non Core.String.is_empty)
  |> List.map ~f:(fun l ->
    let parts = String.split_on_chars ~on:[ ' ' ] l in
    let cs = List.nth_exn parts 0 in
    let ns =
      List.nth_exn parts 1
      |> String.split_on_chars ~on:[ ',' ]
      |> List.map ~f:Int.of_string
    in
    cs, ns)
;;

let sub_at str i = String.sub str ~pos:i ~len:(String.length str - i)

let unfold (cs, ns) =
  ( Sequence.repeat cs
    |> Fn.flip Sequence.take 5
    |> Sequence.fold ~init:"" ~f:(fun acc c -> acc ^ "?" ^ c)
    |> Core.Fn.flip sub_at 1
  , Sequence.repeat ()
    |> Fn.flip Sequence.take 5
    |> Sequence.to_list
    |> List.concat_map ~f:(Fn.const ns) )
;;

let num_arrangements' recurse = function
  | cs, [] -> if String.contains cs '#' then 0 else 1
  | cs, n :: ns ->
    (match Core.String.findi cs ~f:(fun _ c -> not (equal_char c '.')) with
     | None -> 0
     | Some (i, _) ->
       let cs' = sub_at cs i in
       if String.length cs' < n
       then 0
       else (
         let c = String.sub cs' ~pos:0 ~len:n in
         if String.for_all c ~f:(Fn.non (equal_char '.'))
         then
           if String.length cs' = n
           then if List.is_empty ns then 1 else 0
           else if not (equal_char (String.get cs' n) '#')
           then
             (if equal_char (String.get cs' 0) '?'
              then recurse (sub_at cs' 1, n :: ns)
              else 0)
             + recurse (sub_at cs' (n + 1), ns)
           else if equal_char (String.get cs' 0) '#'
           then 0
           else recurse (sub_at cs' 1, n :: ns)
         else if equal_char (String.get cs' 0) '#'
         then 0
         else recurse (sub_at cs' 1, n :: ns)))
;;

type spring = string * int list [@@deriving compare, hash, sexp_of]

let num_arrangements =
  Core.Memo.recursive
    ~hashable:{ hash = hash_spring; compare = compare_spring; sexp_of_t = sexp_of_spring }
    num_arrangements'
;;

let sum_arrangements springs =
  List.map
    ~f:(fun (cs, ns) ->
      let i = num_arrangements (cs, ns) in
      (* Format.printf "--- %d ---\n" i; *)
      i)
    springs
  |> List.fold_left ~f:( + ) ~init:0
;;

let () =
  let test_springs = parse test_input in
  let ic = Stdio.In_channel.create "day12.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let springs = parse input in
  sum_arrangements test_springs |> Stdio.printf "Test 1: %d\n%!";
  sum_arrangements springs |> Stdio.printf "Output 1: %d\n%!";
  sum_arrangements (List.map ~f:unfold test_springs) |> Stdio.printf "Test 2: %d\n%!";
  sum_arrangements (List.map ~f:unfold springs) |> Stdio.printf "Output 2: %d\n%!"
;;
