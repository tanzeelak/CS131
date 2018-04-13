(*1*)
let subset_test0 = subset [1;2] [3;1;2;4];;
(*2*)
let equal_sets_test1 = equal_sets [2;4;5] [5;4;4;2];;
(*3*)
let set_union_test2 = equal_sets (set_union [4;5] [1;2;3]) [4;5;1;2;3];;
(*4*)
let set_intersection_test = equal_sets (set_intersection [1;0;2;3;4] [3;1;4;0]) [3;1;4;0];;
(*5*)
let set_diff_test = not (equal_sets (set_diff [4;3;1] [3]) [1;3;4])
(*6*)
let computed_fixed_point_test = computed_fixed_point (=) sqrt 11. = 1.;;
(*7*)
let computed_periodic_point_test = computed_periodic_point (=) (fun x -> x / 4) 0 (-2) = -2;;
(*8*)
let while_away_test = while_away ((+) 3) ((>) 10) 2;;
(*9*)
let rle_decode_test = rle_decode [4,"w"; 0,"6"; 5, "0"];;
(*10*)
(*let filter_blind_alleys_test g=*)
let filter_blind_alleys_test1 = 
filter_blind_alleys (Expr,
		     [Expr, [N Num];
		     Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
		     Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
		     = (Expr,
			[Expr, [N Num];
			Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
			      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])
