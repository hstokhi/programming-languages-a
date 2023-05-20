(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*1a*)
fun all_except_option(str, lst) =
    case lst of
	[] => NONE
      | hd::tl => case same_string(hd, str) of
		      true => SOME tl
		    | false => case all_except_option(str, tl) of
				   NONE => NONE
				 | SOME tl => SOME (hd::tl)
	   
(*1b*)						 
fun get_substitutions1(lst, str) =
    case lst of
	[] => []
      | hd::tl => case all_except_option(str, hd) of
		      NONE => get_substitutions1(tl, str)
		    | SOME(x) => x @ get_substitutions1(tl, str)
								      
(*1c*)					  
fun get_substitutions2(lst, str) =
    let fun aux(lst, str, acc) =
	    case lst of
		[] => acc
	      | x::xs =>  case all_except_option(str, x) of
			      NONE => aux(xs, str, acc)
			    | SOME(y) => aux(xs, str, acc@y)
    in aux(lst, str, [])
    end
	
(*1d*)
type name = {first: string, middle:string, last:string}
fun similar_names(lst, name) =
    let fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | x::xs => aux(xs, {first=x, middle=(#middle name), last=(#last name)}::acc)
    in
	aux(get_substitutions2(lst, #first name), [name])
    end

	
 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*2a*)
fun card_color(s, r) =
    case (s, r) of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value(s, r) =
    case (s, r) of
	(_, Num(int)) => int
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | hd::tl => case hd = c of
		      true => tl
		    | false => hd::remove_card(tl, c, e)
				      
fun all_same_color(cs) =
    case cs of
	[] => false
      | hd::[] => true
      | hd::(neck::tl) => card_color(hd) = card_color(neck) andalso all_same_color(neck::tl)


fun sum_cards(cs) =
    let fun aux(xs, acc) =
	case xs of
	    [] => acc
	 | hd::tl => aux(tl, acc + card_value(hd))
    in
	aux(cs, 0)
    end
	
fun score(cs, goal) =
    let val gscore = 3 * (sum_cards(cs) - goal)
	val lscore = goal - sum_cards(cs)
    in
	case (sum_cards(cs) > goal, all_same_color(cs)) of
	    (true, true) => gscore div 2
	  | (true, false) => gscore
	  | (false, true) => lscore div 2
	  | (false, false) => lscore
    end

fun officiate(cs, mvs, g) =
    let fun aux(held_cards, card_list, moves, goal) =
	case moves of
	    [] => score(held_cards, goal)
	  | m::ms => case m of
			  Discard(card) => aux(remove_card(held_cards, card, IllegalMove), card_list, ms, goal)
			| Draw => case (card_list, sum_cards(held_cards) > goal) of
				      ([], _) => score(held_cards, goal)
				    | (_, true) => score(held_cards, goal)
				    | (x::xs, _) => aux(x::held_cards, xs, ms, goal)
    in
	aux([], cs, mvs, g)
    end
	
	


