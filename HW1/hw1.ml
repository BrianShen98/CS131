type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec subset a b = match a with
| [] -> true
| x :: xs -> if List.mem x b then subset xs b else false;;

let rec equal_sets a b = subset a b && subset b a ;;

let set_union a b = a @ b;;

let rec set_intersection a b = match a with
|[]->[]
|x :: xs -> if List.mem x b then x :: set_intersection xs b else set_intersection xs b;;

let rec set_diff a b = match a with
|[]->[]
|x :: xs -> if List.mem x b then set_diff xs b else x :: set_diff xs b;; 

let rec computed_fixed_point eq f x = if eq (f x) x then x else computed_fixed_point eq f (f x);;

let rec helperPeriod f p x = match p with
|0 -> x
|_ -> helperPeriod f (p-1) (f x);;

let rec computed_periodic_point eq f p x = if eq x (helperPeriod f p x) then x else computed_periodic_point eq f p (f x);;

let rec while_away s p x = match (p x) with
|false -> []
|true -> x :: while_away s p (s x);;

let rec rle_decode lp = match lp with
|[] -> []
|(a,b) :: xs -> if a = 0 then (rle_decode xs) else b :: rle_decode ((a -1,b)::xs );;


(* x should be of type [] *)
let rec is_All_Terminal x = match x with
|[]-> true
| f::r -> is_All_Terminal r && match f with
|T _ -> true
|N _ -> false;;

let is_nonterminal x = match x with
|N _ -> true
|T _ -> false;;

let in_trace parent tracelist = List.mem parent tracelist;;

(* keys is of [] type*)
let rec map trace_list keys rules = match keys with
|[]->[]
| f::r ->  let children =
let temp = List.map (fun x-> snd x) (List.filter( fun x -> (N (fst x)) = f && (not (in_trace f trace_list))) rules) in 
if temp = [] then [[f]] else temp  in  children :: (map trace_list r rules);;


let rec merge list1 list2 = match list1 with
|[]->[]
|f :: r -> (List.map (fun x-> f@x) list2) @ merge r list2;;


(*combine the output of map function to produce all true children*)


let rec find_all_child  mapList  = match mapList with
|[] -> []
|x1::[]->x1
| x1 :: x2 :: xs -> find_all_child ((merge x1 x2) :: xs);;


let finder_wrapper trace_list curr rules = find_all_child (map trace_list curr rules);;


let rec find_all_starts all_starts rules = match rules with
|[]->all_starts
|f::r -> if List.mem (N(fst f)) all_starts then find_all_starts all_starts r
else find_all_starts (all_starts @ [N(fst f)]) r;;


(* startList requires [[]] type *)
let rec traverse trace_list startList rules = match startList with
|[]->false
|x::xs->
let children = finder_wrapper trace_list x rules in 
if List.exists is_All_Terminal children then true else
if (List.for_all (fun item -> (equal_sets item x)) children) then traverse trace_list xs rules else
let new_trace_list = x @ trace_list in
(traverse trace_list xs rules) || (traverse new_trace_list children rules);;

let routes rules = List.map (fun x -> (traverse [] [[x]] rules , x)) (find_all_starts [] rules);;


let rec add_good_rules good_rules ori_rules all_routes = match ori_rules with
|[]->good_rules
|f::r -> if (not (fst (List.find (fun x-> (snd x) = N (fst f)) all_routes))) || List.exists (fun x -> (is_nonterminal x) && (not (List.mem x (List.map (fun x -> snd x) all_routes)))) (snd f) then
add_good_rules good_rules r all_routes else
add_good_rules (good_rules @ [f]) r all_routes;;


let rec filter_routes good_rules all_routes  =  match all_routes with
|[]-> good_rules
| f :: r -> if not (fst f) then filter_routes (List.filter (fun x-> not (List.mem (snd f) (snd x))) good_rules) r  else
filter_routes good_rules r ;; 

let filter_blind_alleys g = 
(fst g,(let all_routes = routes (snd g) in filter_routes (add_good_rules [] (snd g) all_routes) all_routes));;
