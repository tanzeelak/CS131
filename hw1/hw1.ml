open List
;;

let rec subset a b =
match a with
| [] -> true
| h::t ->if mem h b
then subset t b
else false
;;

let equal_sets a b =
(subset a b) && (subset b a)
;;

let set_union a b = 
a @ b
;;


let rec set_intersection a b =
match a with
| [] -> []
| h::t ->
if (mem h b) then h::(set_intersection t b)
else (set_intersection t b)
;;

let rec set_diff a b =
match a with
| [] -> []
| h::t ->
if (mem h b) then
set_diff t b
else
h::(set_diff t b)
;;                 

(* compare first input x to f f f f f x until equal by eq func must change *)
let rec computed_fixed_point eq f x =
if eq (f x) x 
then x
else computed_fixed_point eq f (f x)
;;

(* return the first repeated element *)
let rec computed_periodic_point eq f p x = 
match p with
| 0 -> x
| _ -> if eq (f(computed_periodic_point eq f (p-1) (f x))) x 
then x
else
computed_periodic_point eq f p (f x)
;;

(* want to return int apparently *)
let rec while_away s p x =
if p x then
x::(while_away s p (s x))  
else
[]
;;

let rec rle_decode lp = 
match lp with
| [] -> []
| (len, num)::t -> 
if len = 0 then rle_decode(t) 
else num :: rle_decode ((len-1,num)::t)
;;

type ('nonterminal, 'terminal) symbol = 
| N of 'nonterminal
| T of 'terminal
;;


type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num
;;

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]
;;

(* check if N "a" is_terminal *)
let is_terminal symbol =
match symbol with
| T _ -> true
| N rhsElem -> false
;;


(* iterate through [N Expr; N Binop; N Expr] and check if all elements are terminal *) 
let rec is_rule_terminal rhs =
match rhs with
| [] -> true
| h::t ->
if is_terminal h then
is_rule_terminal t
else
false
;;

(* iterate through each rule in grammar Expr, [N Expr; N Binop; N Expr]; and add to list *)
let rec find_terminal_rhs rules =
match rules with
| [] -> []
| h::t ->
if is_rule_terminal (snd h) then 
(N (fst h)) :: find_terminal_rhs t
else 
find_terminal_rhs t
;;

(* compare list elements to see if in currList *)
let rec compare_elements_to_terminal_list rhs currList =
match rhs with
| [] -> true
| h::t -> 
if mem h currList then
compare_elements_to_terminal_list t currList
else 
false
;;

(* iterate through the remaining rules and check if rhs of grammar includes an element in the list of t
and then add the lhs to the list of t *)
(*

let rec compare_rhs_terminal_rules rules:(('a * 'b list) list) currList =
match rules with
| [] -> currList
| h::t ->
if compare_elements_to_terminal_list (snd h) currList then
(fst h) :: compare_rhs_terminal_rules t currList
else
compare_rhs_terminal_rules t currList
;;

*)

(*
let filter_blind_alleys g =
let firstRules = find_terminal_rhs (snd g) in
firstRules @ (compare_rhs_terminal_rules (snd g) (find_terminal_rhs firstRules))
 compare_rhs_terminal_rules (snd g) (find_terminal_rhs (snd g))
 check g rhs with list to decide boolean  *)
 
(* it's been three days past the deadline and I still haven't figured it out :( *)
let filter_blind_alleys g = 
g
;;
