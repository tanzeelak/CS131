type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal
;;

type awksub_nonterminals =
| Expr | Term | Lvalue | Incrop | Binop | Num
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

let rec match_rules nonterminal rules =
match rules with
| [] -> []
| headRule :: restRules ->
let lhsRule = fst headRule in
let rhsRule = snd headRule in
if nonterminal = (lhsRule) then
(rhsRule) :: match_rules nonterminal restRules
else
match_rules nonterminal restRules
;;

let convert_grammar g =
let startExp = fst g in
let rules = snd g in
(startExp, (fun nonterminal -> match_rules nonterminal rules))
;;



let is_terminal_rule rule = 
  match rule with
| T x -> true
  | N x -> false
;;


(*


let rec check_symbols gram rules acceptor deriv frag = 
match rules with
| [] -> acceptor deriv frag
| (N headRule::tailRules) ->
//call matcher
| (T headRule::tailRules) ->
  match frag with
  | [] -> None
  | headFrag::tailFrag ->
    if headFrag = headRule then
    check_symbols gram rules acceptor deriv tailFrag
    else None

//Takes rhs [T "3"; N Num] and calls check_symbols on them
let check_rhs rules start acceptor deriv frag = 




let parse_prefix gram frag accept =
match gram with
| (startExp, rules) ->
check_rhs rules startExp acceptor [] frag


| //apply matcher to accept and frag, matcher must return first acceptable match of a prefix of frag
//by trying grammar rules in order
//match succeed if e
if acceptor != None then
acceptor
else
None


*)

convert_grammar (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
;;


let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
	  | Term ->
 [[N Num];
  [N Lvalue];
  [N Incrop; N Lvalue];
  [N Lvalue; N Incrop];
  [T"("; N Expr; T")"]]
     | Lvalue ->
      [[T"$"; N Expr]]
      | Incrop ->
 [[T"++"];
  [T"--"]]
     | Binop ->
      [[T"+"];
        [T"-"]]
	| Num ->
 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
;;


(* prodduc func *) 
(snd awkish_grammar) Expr;;

let produce_nt rules start = rules start;;

(* example *)
produce_nt (snd awkish_grammar) Expr;;


(*
let match_frag gram start derivedList suffix frag accept =
let rhsElem = produce_nt gram start in
match rhsElem with
| [] -> None
| (N nterm)::t ->
match_frag gram nterm rhsElem t frag accept
| (T term)::t ->
if start = term then

else
return None

*)

(*
let rec depMatcher start rules match_start_rules accept deriv frag =

  let rec match_elem rules rule accept deriv frag =
  match rule with
  | [] -> accept deriv frag
  | _ ->
    match frag with
    | [] -> None
    | hPrefix::restPrefix ->
      match rule with
      | [] -> None
      | (T term)::rhs ->
        if hPrefix = term then
        match_elem rules rhs accept deriv restPrefix
        else
        None
      | (N nterm)::rhs ->
      matcher nterm rules (rules nterm) (match_elem rules rhs accept) deriv frag
  in

  let rec matcher start rules match_start_rules accept deriv frag =
  match match_start_rules with
  | [] -> None
  | hRule::restRules ->
    match (match_elem rules hRule accept (deriv@[start, hrule]) frag) with
    | None -> matcher start rules restRules accept deriv frag
    | Some res -> Some res
  in

matcher start rules match_start_rules accept deriv frag
;;

*)

(*
let parse_prefix g =
  let start = (fst g) in
  let rules = (snd g) in
  let match_start_rules = (snd g) (fst g)
depMatcher start rules match_start_rules accept [] frag
;;

*)



let parse_prefix gram accept frag =
  let rec match_element rules rule accept derivation frag = match rule with
  | [] -> accept derivation frag
  | _ -> 
    match frag with
    | [] -> None
    | curr_prefix::r_frag -> 
      match rule with
      | [] -> None
      | (T term)::rhs -> 
        if curr_prefix = term then 
        (match_element rules rhs accept derivation r_frag) 
        else None
      | (N nterm)::rhs ->
        (matcher nterm rules (rules nterm) (match_element rules rhs accept) derivation frag)


  and matcher start rules matching_start_rules accept derivation frag =
  match matching_start_rules with
  | [] -> None
  | top_rule::other_rules ->
    match (match_element rules top_rule accept (derivation@[start, top_rule]) frag) with
    | None -> matcher start rules other_rules accept derivation frag
    | Some res -> Some res
  in
matcher (fst gram) (snd gram) ((snd gram) (fst gram)) accept [] frag
