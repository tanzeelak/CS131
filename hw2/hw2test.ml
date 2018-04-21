let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None


(*
 let includes_zero derivation = function
 | [] -> false
| (_,"0")::_ -> true
| h::t -> includes t
;;

let accept_includes_zero derivation string =
if includes_zero derivation then
Some(derivation, string)
else
None
;;
*)


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

let test_1 = (
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
