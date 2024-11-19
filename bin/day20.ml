open! Core

let test_input = {|broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a|}

type switch =
  | Conjunction of (string, bool) Hashtbl.t * string list
  | Flipflop of bool * string list
  | Broadcast of string list

type outs = string list [@@deriving show]

let parse input =
  let switches =
    String.strip input
    |> String.split ~on:'\n'
    |> List.map ~f:(fun l ->
      let parts = String.split l ~on:'-' in
      let part1 = List.nth_exn parts 1 in
      let outs =
        String.split (String.sub ~pos:2 ~len:(String.length part1 - 2) part1) ~on:','
        |> List.map ~f:String.strip
      in
      let name =
        String.sub (List.hd_exn parts) ~pos:1 ~len:(String.length (List.hd_exn parts) - 2)
      in
      match String.get l 0 with
      | 'b' -> "broadcast", Broadcast outs
      | '%' -> name, Flipflop (false, outs)
      | '&' -> name, Conjunction (Hashtbl.create (module String), outs)
      | _ -> failwith "unhandled module type")
  in
  let table = Hashtbl.of_alist_exn (module String) switches in
  List.iter switches ~f:(fun (name, switch) ->
    let outs =
      match switch with
      | Conjunction (_, outs) -> outs
      | Flipflop (_, outs) -> outs
      | Broadcast outs -> outs
    in
    List.iter outs ~f:(fun out ->
      Hashtbl.change table out ~f:(fun out_val ->
        match out_val with
        | Some (Conjunction (ins, outs)) ->
          Hashtbl.add_exn ins ~key:name ~data:false;
          Some (Conjunction (ins, outs))
        | Some other -> Some other
        | None -> None)));
  table
;;

let rec process' switches (low_count, high_count) src' dest' wrote_to_dest = function
  | [] -> low_count, high_count, wrote_to_dest
  | (src, dest, high) :: ps ->
    (* Stdio.printf "%s %s %b\n" src dest high; *)
    (match Hashtbl.find switches dest with
     | None ->
       process'
         switches
         (low_count, high_count)
         src'
         dest'
         (if List.mem src' src ~equal:equal_string && equal_string dest dest' && high
          then List.Assoc.add wrote_to_dest ~equal:equal_string src true
          else wrote_to_dest)
         ps
     | Some (Broadcast outs) ->
       process'
         switches
         ( (low_count + if not high then List.length outs else 0)
         , high_count + if high then List.length outs else 0 )
         src'
         dest'
         (if List.mem src' src ~equal:equal_string && equal_string dest dest' && high
          then List.Assoc.add wrote_to_dest ~equal:equal_string src true
          else wrote_to_dest)
         (ps @ List.map outs ~f:(fun out -> dest, out, high))
     | Some (Flipflop (high', outs)) ->
       if high
       then
         process'
           switches
           (low_count, high_count)
           src'
           dest'
           (if List.mem src' src ~equal:equal_string && equal_string dest dest' && high
            then List.Assoc.add wrote_to_dest ~equal:equal_string src true
            else wrote_to_dest)
           ps
       else (
         Hashtbl.update switches dest ~f:(fun _ -> Flipflop (not high', outs));
         process'
           switches
           ( (low_count + if high' then List.length outs else 0)
           , high_count + if not high' then List.length outs else 0 )
           src'
           dest'
           (if List.mem src' src ~equal:equal_string && equal_string dest dest' && high
            then List.Assoc.add wrote_to_dest ~equal:equal_string src true
            else wrote_to_dest)
           (ps @ List.map outs ~f:(fun out -> dest, out, not high')))
     | Some (Conjunction (ins, outs)) ->
       Hashtbl.update ins src ~f:(fun _ -> high);
       if Hashtbl.for_all ins ~f:Fn.id
       then
         process'
           switches
           (low_count + List.length outs, high_count)
           src'
           dest'
           (if List.mem src' src ~equal:equal_string && equal_string dest dest' && high
            then List.Assoc.add wrote_to_dest ~equal:equal_string src true
            else wrote_to_dest)
           (ps @ List.map outs ~f:(fun out -> dest, out, false))
       else
         process'
           switches
           (low_count, high_count + List.length outs)
           src'
           dest'
           (if List.mem src' src ~equal:equal_string && equal_string dest dest' && high
            then List.Assoc.add wrote_to_dest ~equal:equal_string src true
            else wrote_to_dest)
           (ps @ List.map outs ~f:(fun out -> dest, out, true)))
;;

let process switches src' dest' =
  process' switches (1, 0) src' dest' [] [ "button", "broadcast", false ]
;;

let rec process_1000' switches i (low_count, high_count) =
  if i = 1000
  then low_count, high_count
  else (
    let low_count', high_count', _ = process switches [] "" in
    (* Stdio.printf "%d %d\n" low_count' high_count'; *)
    process_1000' switches (i + 1) (low_count + low_count', high_count + high_count'))
;;

let process_1000 switches = process_1000' switches 0 (0, 0)

let rec num_until switches i src' dest' first_wrote_to_dest =
  if i % 1_000_000 = 0 then Stdio.printf "%d\n%!" i;
  let _, _, wrote_to_dest = process switches src' dest' in
  let first_wrote_to_dest' =
    first_wrote_to_dest
    @ List.filter_map wrote_to_dest ~f:(fun (d, _) ->
      if List.exists first_wrote_to_dest ~f:(fun (d', _) -> equal_string d d')
      then None
      else Some (d, i))
  in
  if List.length first_wrote_to_dest' = List.length src'
  then List.map first_wrote_to_dest' ~f:Tuple2.get2
  else num_until switches (i + 1) src' dest' first_wrote_to_dest'
;;

let rec gcd a = function
  | 0 -> a
  | b -> gcd b (a mod b)
;;

let lcm x y = x * y / gcd x y

let num_until_low_rx switches =
  let writes_to_rx =
    Hashtbl.fold switches ~init:[] ~f:(fun ~key ~data acc ->
      if List.mem
           (match data with
            | Conjunction (_, outs) -> outs
            | Flipflop (_, outs) -> outs
            | Broadcast outs -> outs)
           "rx"
           ~equal:equal_string
      then acc @ [ key ]
      else acc)
    |> List.hd_exn
  in
  let writes_to_writes_to_rx =
    Hashtbl.fold switches ~init:[] ~f:(fun ~key ~data acc ->
      if List.mem
           (match data with
            | Conjunction (_, outs) -> outs
            | Flipflop (_, outs) -> outs
            | Broadcast outs -> outs)
           writes_to_rx
           ~equal:equal_string
      then acc @ [ key ]
      else acc)
  in
  num_until switches 1 writes_to_writes_to_rx writes_to_rx [] |> List.fold ~init:1 ~f:lcm
;;

let () =
  let ic = Stdio.In_channel.create "day20.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_switches = parse test_input in
  let switches = parse input in
  process_1000 test_switches
  |> (fun (low, high) -> low * high)
  |> Stdio.printf "Test 1: %d\n%!";
  process_1000 switches
  |> (fun (low, high) -> low * high)
  |> Stdio.printf "Output 1: %d\n%!";
  let switches = parse input in
  num_until_low_rx switches |> Stdio.printf "Output 2: %d\n%!"
;;
