open! Core

let test_input = {|rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7|}
let parse input = String.split ~on:',' (String.strip input)

let hash_algorithm input =
  String.fold input ~init:0 ~f:(fun acc c -> (acc + int_of_char c) * 17 % 256)
;;

let sum_hashes steps = List.map steps ~f:hash_algorithm |> List.fold ~init:0 ~f:( + )

let arrange steps =
  let boxes = Sequence.repeat [] |> Fn.flip Sequence.take 256 |> Sequence.to_list in
  List.fold_left steps ~init:boxes ~f:(fun bs step ->
    if String.contains step '-'
    then (
      let parts = String.split step ~on:'-' in
      let label = List.hd_exn parts in
      let hash = hash_algorithm label in
      List.mapi bs ~f:(fun i b ->
        if i = hash
        then List.filter b ~f:(fun (label', _) -> not (equal_string label' label))
        else b))
    else (
      let parts = String.split step ~on:'=' in
      let label = List.hd_exn parts in
      let length = List.nth_exn parts 1 |> int_of_string in
      let hash = hash_algorithm label in
      List.mapi bs ~f:(fun i b ->
        if i = hash
        then
          if List.exists b ~f:(fun (label', _) -> equal_string label' label)
          then
            List.map b ~f:(fun (label', length') ->
              if equal_string label' label then label', length else label', length')
          else b @ [ label, length ]
        else b)))
;;

type boxes = (string * int) list list [@@deriving show]

let focusing_power boxes =
  Stdio.printf "%s\n" (show_boxes boxes);
  List.foldi boxes ~init:0 ~f:(fun i acc box ->
    acc
    + List.foldi box ~init:0 ~f:(fun j acc (_, length) ->
      acc + ((i + 1) * (j + 1) * length)))
;;

let () =
  let test_steps = parse test_input in
  let ic = Stdio.In_channel.create "day15.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let steps = parse input in
  sum_hashes test_steps |> Stdio.printf "Test 1: %d\n%!";
  sum_hashes steps |> Stdio.printf "Output 1: %d\n%!";
  arrange test_steps |> focusing_power |> Stdio.printf "Test 2: %d\n%!";
  arrange steps |> focusing_power |> Stdio.printf "Output 2: %d\n%!"
;;
