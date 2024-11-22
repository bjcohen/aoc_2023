open! Core

let test_input =
  {|jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr|}
;;

type nodes_alist = (string * string list) list [@@deriving show]
type nodes_list = (string * string) list [@@deriving show]
type string_list = string list [@@deriving show]

type node =
  { label : string
  ; mutable parent : node option
  ; mutable size : int
  ; rank : int
  }
[@@deriving equal]

(* let show_node node = *)
(*   Format.sprintf *)
(*     "node{label=%s, parent=%s, size=%d}" *)
(*     node.label *)
(*     (Option.value_exn node.parent).label *)
(*     node.size *)
(* ;; *)

let make label =
  let n = { label; parent = None; size = 1; rank = 0 } in
  n.parent <- Some n;
  n
;;

let find n =
  let n' = ref n in
  while not (equal_node (Option.value_exn !n'.parent) !n') do
    !n'.parent <- Some (Option.value_exn (Option.value_exn !n'.parent).parent);
    n' := Option.value_exn !n'.parent
  done;
  !n'
;;

let union x y =
  let x = find x in
  let y = find y in
  if equal_node x y
  then ()
  else if x.size < y.size
  then (
    x.parent <- Some y;
    y.size <- x.size + y.size)
  else (
    y.parent <- Some x;
    x.size <- x.size + y.size)
;;

let parse input =
  let nodes = Hashtbl.create (module String) in
  input
  |> String.strip
  |> String.split_lines
  |> List.iter ~f:(fun l ->
    let k = String.sub l ~pos:0 ~len:3 in
    let vs = String.sub l ~pos:5 ~len:(String.length l - 5) |> String.split ~on:' ' in
    List.iter vs ~f:(fun v ->
      let _ = Hashtbl.add_multi nodes ~key:k ~data:v in
      let _ = Hashtbl.add_multi nodes ~key:v ~data:k in
      ()));
  nodes
;;

let rec compute_random_cut2'
  n_cliques
  edges
  (cut : (string, node * string list) Hashtbl.t)
  =
  if n_cliques = 2
  then cut
  else (
    (* Stdio.printf "%d\n%!" n_cliques; *)
    (* let src, (_node, dsts) = Hashtbl.choose_randomly_exn cut in *)
    (* let dst = List.random_element_exn dsts in *)
    let (src, dst), () = Hashtbl.choose_randomly_exn edges in
    Hashtbl.update cut src ~f:(fun data ->
      let node, dsts = Option.value_exn data in
      node, List.filter dsts ~f:(fun dst' -> not (equal_string src dst')));
    Hashtbl.update cut dst ~f:(fun data ->
      let node, dsts = Option.value_exn data in
      node, List.filter dsts ~f:(fun dst' -> not (equal_string dst dst')));
    let src_node, _ = Hashtbl.find_exn cut src in
    let dst_node, _ = Hashtbl.find_exn cut dst in
    let is_merge = not (equal_string (find src_node).label (find dst_node).label) in
    union src_node dst_node;
    compute_random_cut2' (if is_merge then n_cliques - 1 else n_cliques) edges cut)
;;

module StringPair = struct
  type t = string * string [@@deriving hash, compare, sexp]
end

let compute_random_cut2 (nodes : (string, string list) Hashtbl.t) =
  compute_random_cut2'
    (Hashtbl.length nodes)
    (Hashtbl.fold
       nodes
       ~init:(Hashtbl.create (module StringPair))
       ~f:(fun ~key ~data acc ->
         List.iter data ~f:(fun d -> Hashtbl.set acc ~key:(key, d) ~data:());
         acc))
    (Hashtbl.mapi nodes ~f:(fun ~key:src ~data:dsts -> make src, dsts))
;;

let rec min_partition_sizes_product2 ?(i = 0) nodes =
  (* if i % 10 = 0 then Stdio.printf "--- %d ---\n%!" i; *)
  let cut = compute_random_cut2 nodes in
  (* Stdio.printf "Cut:\n"; *)
  (* Hashtbl.iteri cut ~f:(fun ~key:src ~data:(node, dsts) -> *)
  (*   Stdio.printf "%s %s (%s) -> %s\n%!" src (show_node node) (find node).label (show_string_list dsts)); *)
  let n_between_edges =
    Hashtbl.fold cut ~init:0 ~f:(fun ~key:_ ~data:(node, dsts) acc ->
      List.count dsts ~f:(fun dst ->
        not
          (equal_string
             (find node).label
             (find (Tuple2.get1 (Hashtbl.find_exn cut dst))).label))
      + acc)
  in
  (* Stdio.printf "n_between=%d\n%!" n_between_edges; *)
  if n_between_edges = 6
  then (
    let counter = Hashtbl.create (module String) in
    Hashtbl.iter cut ~f:(fun (node, _edges) ->
      Hashtbl.update counter (find node).label ~f:(fun c_opt ->
        Option.value c_opt ~default:0 + 1));
    Hashtbl.fold counter ~init:1 ~f:(fun ~key:_ ~data acc -> acc * data))
  else min_partition_sizes_product2 ~i:(i + 1) nodes
;;

let rec _compute_random_cut'
  (cut : (string, string Hash_set.t * (string, int) Hashtbl.t) Hashtbl.t)
  =
  if Hashtbl.length cut = 2
  then cut
  else (
    (* Stdio.print_endline "Cut:\n"; *)
    (* Hashtbl.iteri cut ~f:(fun ~key:src ~data:(merged, dsts) -> *)
    (*   Stdio.printf *)
    (*     "%s %s -> %s\n" *)
    (*     src *)
    (*     (show_string_list (Hash_set.to_list merged)) *)
    (*     (show_string_list dsts)); *)
    let edges =
      Hashtbl.fold cut ~init:[] ~f:(fun ~key ~data:(_, dsts) acc ->
        Hashtbl.fold dsts ~init:acc ~f:(fun ~key:d ~data:_ acc -> acc @ [ key, d ]))
    in
    if List.is_empty edges
    then cut
    else (
      let src, dst = List.random_element_exn edges in
      Hashtbl.update cut src ~f:(fun cnt_dsts_opt ->
        let merged, dsts = Option.value_exn cnt_dsts_opt in
        let dst_merged, dst_dsts = Hashtbl.find_exn cut dst in
        Hash_set.add merged dst;
        ( Hash_set.union merged dst_merged
        , Hashtbl.merge
            (Hashtbl.filteri dsts ~f:(fun ~key:d ~data:_ -> not (equal_string d dst)))
            (Hashtbl.filteri dst_dsts ~f:(fun ~key:d ~data:_ -> not (equal_string d src)))
            ~f:(fun ~key:_ data ->
              match data with
              | `Left c -> Some c
              | `Right c -> Some c
              | `Both (c1, c2) -> Some (c1 + c2)) ));
      Hashtbl.map_inplace
        cut
        ~f:
          (Tuple2.map_snd ~f:(fun dsts ->
             let dst_count = Hashtbl.find_and_remove dsts dst in
             if Option.is_some dst_count
             then
               Hashtbl.update dsts src ~f:(fun c_opt ->
                 Option.value c_opt ~default:0 + Option.value_exn dst_count);
             dsts));
      Hashtbl.remove cut dst;
      _compute_random_cut' cut))
;;

let _compute_random_cut (nodes : (string, string list) Hashtbl.t) =
  Hashtbl.map nodes ~f:(fun edges ->
    ( Hash_set.create (module String)
    , Hashtbl.of_alist_exn (module String) (List.map edges ~f:(fun e -> e, 1)) ))
  |> _compute_random_cut'
;;

let rec _min_partition_sizes_product ?(i = 0) nodes =
  if i % 1 = 0 then Stdio.printf "%d\n%!" i;
  let cut = _compute_random_cut nodes in
  if Hashtbl.length cut = 2
     && Hashtbl.fold cut ~init:0 ~f:(fun ~key:_ ~data:(_nodes, edges) acc ->
          Hashtbl.fold edges ~init:0 ~f:(fun ~key:_key ~data acc -> acc + data) + acc)
        = 6
  then
    Hashtbl.fold cut ~init:1 ~f:(fun ~key:_ ~data:(merged, _edges) acc ->
      acc * (1 + Hash_set.length merged))
  else _min_partition_sizes_product ~i:(i + 1) nodes
;;

let () =
  let ic = Stdio.In_channel.create "day25.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_nodes = parse test_input in
  let nodes = parse input in
  min_partition_sizes_product2 test_nodes |> Stdio.printf "Test 1: %d\n%!";
  min_partition_sizes_product2 nodes |> Stdio.printf "Output 1: %d\n%!"
;;
