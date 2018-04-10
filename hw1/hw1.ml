open List;;

let rec subset a b =
match a with
| [] -> true
| h::t ->if mem h b
then subset t b
else false;;

let equal_sets a b =
(subset a b) && (subset b a);;

let rec sort a =
match a with
[] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt a =
   match a with
     [] -> [elt]
   | head :: tail -> if elt <= head then elt :: a else head :: insert elt tail;;

let rec uniq a =
match a with
    | [] -> []
    | [_] -> a
    | h1 :: ((h2 :: _) as tail) ->
        if h1 = h2 then uniq tail else h1 :: uniq tail

let set_union_old a b = 
  uniq(sort(a@b));;

let set_union a b = 
a@b;;


let rec set_intersection l1 l2 =
   let rec contains i l = match l with
     | [] -> false
   | h::t -> if i = h then true else contains i t
   in
  match l1 with
  | [] -> []
  | h::t -> if (contains h l2) then h::(set_intersection t l2) else set_intersection t l2;;

let set_diff_old a b =
  filter(fun x -> not (mem x (set_intersection a b))) (set_union a b);;

let set_diff a b =
  let func1 x =
  not (mem x (set_intersection a b))
  in
  filter func1 (set_union a b);;


* compare first input x to f f f f f x until equal by eq func must change * 
let rec computed_fixed_point eq f x =
  if eq (f x) x 
  then x
  else computed_fixed_point eq f (f x);;

* return the first repeated element *
let rec computed_periodic_point eq f p x = 
match p with
| 0 -> x
| _ -> if eq (f(computed_periodic_point eq f (p-1) (f x))) x 
then x
else
computed_periodic_point eq f p (f x);;

* want to return int apparently *
let rec while_away s p x =
  if p x then
  x :: (while_away s p (s x))  
  else
  [];;

let rec rle_decode lp = 
match lp with
[] -> []
| (len, num)::t -> if len = 0 then rle_decode(t) else val::rle_decode ((len-1,val)::t);;

let rec rle_decode lp = match lp with
[] -> []
    | (num, value)::t -> if num = 0 then rle_decode(t) else value::rle_decode ((num-1,value)::t);;    
val rle_de


let rec rle_decode lp = 
match lp with
| [] -> []
| (len, num)::t -> 
if len = 0 then rle_decode(t) 
else num :: rle_decode ((len-1,num)::t);; 

type ('nonterminal, 'terminal) symbol = 
| N of 'nonterminal
| T of 'terminal ;;
