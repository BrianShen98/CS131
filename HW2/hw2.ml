type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Question 1 *)
let hw2_rules ori_rules symbol = List.filter (fun x -> fst x = symbol) ori_rules;;

let convert_grammar gram1 = (fst gram1, hw2_rules (snd gram1));;

(* Question 2 *)

let match_empty deriv accept frag = accept deriv frag;;
let match_nothing deriv accept frag = None;;

let match_terminal tmnl deriv accept frag = match frag with
|[]-> None
|head :: tail -> if tmnl = (T head) then accept deriv tail else None;;

let append_matcher matcher1 matcher2 deriv accept frag =
matcher1 deriv (fun d -> fun frag1 -> (matcher2 d accept frag1)) frag;;

(*append multiple matchers to form a matcher*)

let rec make_and_matchers prod_func = function
|(T head) :: tail -> fun deriv accept frag -> append_matcher (match_terminal (T head)) (make_and_matchers prod_func tail) deriv accept frag
|(N head) :: tail -> fun deriv accept frag ->
append_matcher (make_or_matcher prod_func head (prod_func head)) (make_and_matchers prod_func tail) deriv accept frag
|[] -> match_empty

(*choose all possible matchers from left to right*)

and make_or_matcher prod_func start_symbl = function 
|[] -> match_nothing
|head::tail -> (fun deriv accept frag -> let ormatch = make_and_matchers prod_func head (deriv @ [(start_symbl,head)]) accept frag 
	in match ormatch with
		  |None -> make_or_matcher prod_func start_symbl tail deriv accept frag
                  |Some(d,s) -> Some(d ,s));;   

let parse_prefix gram = let prod_func = snd gram and start_symbl = fst gram in 
fun accept frag -> make_or_matcher prod_func start_symbl (prod_func start_symbl) [] accept frag;;
