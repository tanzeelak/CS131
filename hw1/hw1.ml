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
