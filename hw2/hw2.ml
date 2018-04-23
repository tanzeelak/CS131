type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal
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


let produceNT rules start = rules start
;;

let rec matchXRules start rules currNT derivList accept frag =
match currNT with
| [] -> None
| headRule::restRules ->
  let newDerivList = derivList@[(start, headRule)] in
  match (matchYElem headRule rules accept newDerivList frag) with
  | None -> matchXRules start rules restRules derivList accept frag
  | Some(d,s) -> Some(d,s)

and 

matchYElem currRule rules accept derivList frag =
match currRule with
| [] -> accept derivList frag
| (T headRHSElem)::restRHSElem ->
  ( (* Terminal element so we match the prefix of the fragment recursively *)
  match frag with
  | [] -> None
  | headPrefix::restPrefixes ->
      if headRHSElem = headPrefix then
      matchYElem restRHSElem rules accept derivList restPrefixes
      else None)
| (N headRHSElem)::restRHSElem ->
  (let currNT = produceNT rules headRHSElem in
  let outerAccept = matchYElem restRHSElem rules accept in
  matchXRules headRHSElem rules currNT derivList outerAccept frag)
;;

let parse_prefix gram accept frag = 
  let start = (fst gram) in
  let rules = (snd gram) in
  let currNT = produceNT (snd gram) (fst gram) in
matchXRules start rules currNT [] accept frag
;;
