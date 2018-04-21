type awksub_nonterminals =
| Expr | Term | Lvalue | Incrop | Binop | Num
;;

let accept_all derivation string = Some (derivation, string)
;;

let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None
;;

let rec accept_includes_zero derivation string =
match string with 
|[] -> None
|head::tail ->
if head = "0" then
Some (derivation)
else
accept_includes_zero derivation tail
;;


let rec accept_number5 derivation = function
        |[] -> None
        |head::tail -> if head = "5" then Some (derivation)
else accept_number5 derivation tail
;;

let g1 = 
(Expr,
 function
      | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
     [[N Num; T"%"];
     [N Num];
     [N Lvalue];
     [N Incrop; N Lvalue];
     [N Lvalue; N Incrop];
     [T"("; N Expr; T")"]];
          | Lvalue ->
 [[T"$"; N Expr]]
     | Incrop ->
     [[T"++"];
     [T"--"]];
          | Binop ->
 [[T"+"];
  [T"-"];
[T"/"];
[T"*"]];
     | Num ->
     [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
     [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
;;

let test_1 = (parse_prefix g1 accept_empty_suffix ["4";"%";"/";"8";"-";"("; "$"; "8"; "++"; ")"] != None) ;;

let test_2 = (
parse_prefix g1 accept_all ["4";"%";"/";"8";"-";"("; "$"; "8"; "++"; ")"]
= 
Some
 ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num; T "%"]); (Num, [T "4"]);
   (Binop, [T "/"]); (Expr, [N Term; N Binop; N Expr]); (Term, [N Num]);
   (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]);
   (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
   (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
   (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"])],
  [])
)
;;
