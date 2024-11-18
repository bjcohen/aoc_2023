open! Core

let test_input =
  {|px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}|}
;;

(* 178623015427702 *)
(* 178623015427702 *)

type rule =
  | Always of string
  | Cond of char * char * int * string

module RuleName = struct
  type t = string [@@deriving hash, compare, sexp]
end

let parse input =
  let workflows, ratings' =
    String.strip input
    |> String.split ~on:'\n'
    |> List.split_while ~f:(Fn.non String.is_empty)
  in
  ( List.map workflows ~f:(fun l ->
      Scanf.sscanf l "%[a-z]{%[^{}]}" (fun name rules ->
        ( name
        , String.split rules ~on:','
          |> List.map ~f:(fun rule ->
            if String.contains rule ':'
            then (
              let rule_parts = String.split rule ~on:':' in
              let part1 = List.hd_exn rule_parts in
              Cond
                ( String.get part1 0
                , String.get part1 1
                , int_of_string (String.sub part1 ~pos:2 ~len:(String.length part1 - 2))
                , List.nth_exn rule_parts 1 ))
            else Always rule) )))
    |> Hashtbl.of_alist_exn (module RuleName)
  , List.map (List.tl_exn ratings') ~f:(fun l ->
      Scanf.sscanf l "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> x, m, a, s)) )
;;

let rec apply_rules rules ((x, m, a, s) as part) rule_name =
  let rule = Hashtbl.find_exn rules rule_name in
  let next_rule_name =
    List.find_map_exn rule ~f:(function
      | Always next_rule_name -> Some next_rule_name
      | Cond (cat, comp_operator, n, next_rule_name) ->
        if (match comp_operator with
            | '>' -> ( > )
            | '<' -> ( < )
            | _ -> failwith "unhandled comp_operator")
             (match cat with
              | 'x' -> x
              | 'm' -> m
              | 'a' -> a
              | 's' -> s
              | _ -> failwith "unhandled cat")
             n
        then Some next_rule_name
        else None)
  in
  match next_rule_name with
  | "A" -> true
  | "R" -> false
  | _ -> apply_rules rules part next_rule_name
;;

let total_accepted_rating rules parts =
  List.filter parts ~f:(fun part -> apply_rules rules part "in")
  |> List.map ~f:(fun (x, m, a, s) -> x + m + a + s)
  |> List.fold ~init:0 ~f:( + )
;;

let get_num_allowed_numbers conds =
  let min_conds =
    List.filter_map conds ~f:(fun (op, n) -> if equal_char op '>' then Some n else None)
  in
  let max_conds =
    List.filter_map conds ~f:(fun (op, n) -> if equal_char op '<' then Some n else None)
  in
  let max_min_cond = List.max_elt min_conds ~compare:Int.compare in
  let min_max_cond = List.min_elt max_conds ~compare:Int.compare in
  match max_min_cond, min_max_cond with
  | Some min, Some max -> if max >= min then max - min - 1 else 0
  | Some min, None -> 4000 - min
  | None, Some max -> max - 1
  | None, None -> 4000
;;

type cond_list = (char * int) list [@@deriving show]

let flip_condition comp_operator n =
  match comp_operator with
  | '>' -> '<', n + 1
  | '<' -> '>', n - 1
  | _ -> failwith "unhandled comp_operator"
;;

let flip_conditions conditions i cat =
  List.take conditions i
  |> List.filter_map ~f:(function
    | Cond (cat', comp_operator', n', _) ->
      if equal_char cat' cat then Some (flip_condition comp_operator' n') else None
    | _ -> failwith "unhandled condition type")
;;

let rec num_accepted_rating_combinations'
  rules
  curr_rule_name
  x_conds
  m_conds
  a_conds
  s_conds
  =
  if equal_string curr_rule_name "A"
  then
    get_num_allowed_numbers x_conds
    * get_num_allowed_numbers m_conds
    * get_num_allowed_numbers a_conds
    * get_num_allowed_numbers s_conds
  else if equal_string curr_rule_name "R"
  then 0
  else (
    let conditions = Hashtbl.find_exn rules curr_rule_name in
    List.mapi conditions ~f:(fun i condition ->
      match condition with
      | Always nrn ->
        num_accepted_rating_combinations'
          rules
          nrn
          (x_conds @ flip_conditions conditions i 'x')
          (m_conds @ flip_conditions conditions i 'm')
          (a_conds @ flip_conditions conditions i 'a')
          (s_conds @ flip_conditions conditions i 's')
      | Cond (cat, comp_operator, n, next_rule_name) ->
        (match cat with
         | 'x' ->
           num_accepted_rating_combinations'
             rules
             next_rule_name
             (((comp_operator, n) :: x_conds) @ flip_conditions conditions i 'x')
             (m_conds @ flip_conditions conditions i 'm')
             (a_conds @ flip_conditions conditions i 'a')
             (s_conds @ flip_conditions conditions i 's')
         | 'm' ->
           num_accepted_rating_combinations'
             rules
             next_rule_name
             (x_conds @ flip_conditions conditions i 'x')
             (((comp_operator, n) :: m_conds) @ flip_conditions conditions i 'm')
             (a_conds @ flip_conditions conditions i 'a')
             (s_conds @ flip_conditions conditions i 's')
         | 'a' ->
           num_accepted_rating_combinations'
             rules
             next_rule_name
             (x_conds @ flip_conditions conditions i 'x')
             (m_conds @ flip_conditions conditions i 'm')
             (((comp_operator, n) :: a_conds) @ flip_conditions conditions i 'a')
             (s_conds @ flip_conditions conditions i 's')
         | 's' ->
           num_accepted_rating_combinations'
             rules
             next_rule_name
             (x_conds @ flip_conditions conditions i 'x')
             (m_conds @ flip_conditions conditions i 'm')
             (a_conds @ flip_conditions conditions i 'a')
             (((comp_operator, n) :: s_conds) @ flip_conditions conditions i 's')
         | _ -> failwith "unhandled cat"))
    |> List.fold ~init:0 ~f:( + ))
;;

let num_accepted_rating_combinations rules =
  num_accepted_rating_combinations' rules "in" [] [] [] []
;;

let () =
  let ic = Stdio.In_channel.create "day19.txt" in
  let input = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  let test_rules, test_parts = parse test_input in
  let rules, parts = parse input in
  total_accepted_rating test_rules test_parts |> Stdio.printf "Test 1: %d\n%!";
  total_accepted_rating rules parts |> Stdio.printf "Output 1: %d\n%!";
  num_accepted_rating_combinations test_rules |> Stdio.printf "Test 2: %d\n%!";
  num_accepted_rating_combinations rules |> Stdio.printf "Output 2: %d\n%!"
;;
