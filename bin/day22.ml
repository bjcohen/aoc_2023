open! Core

let test_input =
  {|1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9|}
;;

let parse input =
  String.strip input
  |> String.split ~on:'\n'
  |> List.map ~f:(fun l ->
    let[@warning "-8"] [ x1; y1; z1; x2; y2; z2 ] =
      String.split_on_chars l ~on:[ ','; '~' ] |> List.map ~f:int_of_string
    in
    let _ = assert (x1 <= x2 && y1 <= y2 && z1 <= z2) in
    x1, y1, z1, x2, y2, z2)
;;

let intersect x1 x2 x1' x2' = x2 >= x1' && x2' >= x1

let settle bricks =
  let bricks' =
    List.sort bricks ~compare:(fun (_, _, z1, _, _, _) (_, _, z1', _, _, _) ->
      Int.compare z1 z1')
  in
  List.fold bricks' ~init:[] ~f:(fun acc (x1, y1, z1, x2, y2, z2) ->
    let maxz =
      acc
      |> List.filter_map ~f:(fun (x1', y1', _z1', x2', y2', z2') ->
        if intersect x1 x2 x1' x2' && intersect y1 y2 y1' y2' then Some z2' else None)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    acc @ [ x1, y1, maxz + 1, x2, y2, z2 - z1 + maxz + 1 ])
;;

let num_disintegrated bricks =
  let supporting_bricks =
    List.mapi bricks ~f:(fun i (x1, y1, z1, x2, y2, _z2) ->
      List.take bricks i
      |> List.filter_mapi ~f:(fun j (x1', y1', _z1', x2', y2', z2') ->
        if intersect x1 x2 x1' x2' && intersect y1 y2 y1' y2' && z2' + 1 = z1
        then Some j
        else None))
  in
  List.length bricks
  - (List.filter_map supporting_bricks ~f:(fun b ->
       if List.length b = 1 then Some (List.hd_exn b) else None)
     |> List.dedup_and_sort ~compare:Int.compare
     |> List.length)
;;

type int_list = int list [@@deriving show]
type int_list_list = int list list [@@deriving show]

let total_disintegrated bricks =
  let supporting_bricks =
    List.mapi bricks ~f:(fun i (x1, y1, z1, x2, y2, _z2) ->
      List.take bricks i
      |> List.filter_mapi ~f:(fun j (x1', y1', _z1', x2', y2', z2') ->
        if intersect x1 x2 x1' x2' && intersect y1 y2 y1' y2' && z2' + 1 = z1
        then Some j
        else None))
  in
  (* Stdio.printf "%s\n" (show_int_list_list supporting_bricks); *)
  let disint_counts =
    List.mapi supporting_bricks ~f:(fun i _ ->
      (List.foldi supporting_bricks ~init:[] ~f:(fun i' acc supporting_bricks ->
         if i' < i
         then acc @ [ false ]
         else if i' = i
         then acc @ [ true ]
         else
           acc
           @ [ (if List.is_empty supporting_bricks
                then false
                else List.for_all supporting_bricks ~f:(fun b -> List.nth_exn acc b))
             ])
       |> List.count ~f:Fn.id)
      - 1)
  in
  (* Stdio.printf "%s\n" (show_int_list disint_counts); *)
  List.fold disint_counts ~init:0 ~f:( + )
;;

let () =
  let ic = Stdio.In_channel.create "day22.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_bricks = parse test_input in
  let bricks = parse input in
  settle test_bricks |> num_disintegrated |> Stdio.printf "Test 1: %d\n%!";
  settle bricks |> num_disintegrated |> Stdio.printf "Output 1: %d\n%!";
  settle test_bricks |> total_disintegrated |> Stdio.printf "Test 2: %d\n%!";
  settle bricks |> total_disintegrated |> Stdio.printf "Output 2: %d\n%!"
;;
