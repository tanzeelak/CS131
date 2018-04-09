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
