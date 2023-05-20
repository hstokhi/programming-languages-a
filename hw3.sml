(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** for the challenge problem only ****)
	
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals string_list =
    List.filter (fn str => Char.isUpper(String.sub(str, 0))) string_list

fun longest_string1 (string_list) =
    foldl (fn (x, y) => if String.size x > String.size y then x else y) "" string_list

fun longest_string2 string_list =
    foldl (fn (x,y) => if (size x) >= (size y) then x else y) "" string_list

fun longest_string_helper f string_list =
    foldl (fn (x,y) => if f(size x, size y) then x else y) "" string_list

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string_helper (fn(x,y) => x >= y) o only_capitals

(*rev l
returns a list consisting of l's elements in reverse order.

 implode l
generates the string containing the characters in the list l. This is equivalent to concat (List.map str l). This raises Size if the resulting string would have size greater than maxSize.

explode s
is the list of characters in the string s.*)

val rev_string = implode o rev o explode

fun first_answer func args =
    case args of
	[] => raise NoAnswer
      | x::xs => case func x of
		     NONE => first_answer func xs
		   | SOME y => y

fun all_answers func args =
    let fun helper (func, args, acc) =
	    case args of
		[] => SOME acc
	      | x::xs => case func x of
			     NONE => NONE
			   | SOME y => helper(func, xs, acc@y)
    in
	helper(func, args, [])
    end

val count_wildcards = g (fn () => 1)(fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) size
fun count_some_var (str, ptrn) = g (fn () => 0) (fn x => if str = x then 1 else 0) ptrn
				   
val check_pat =
    let fun var_list pattern = case pattern of
				    Variable p => [p]
				  | TupleP ps => foldl (fn (p, acc) => var_list(p)@acc) [] ps
				  | ConstructorP (_, p) => var_list(p)
				  | _ => []
	fun no_repeats string_list = case string_list of
					[] => true
				      | x::xs => not (List.exists (fn y => y = x) xs) andalso no_repeats(xs)
    in
	no_repeats o var_list
    end
	
fun match (v, p) = case (v, p) of
		    (_, Wildcard) => SOME []
		  | (sv, Variable sp) => SOME [(sp, sv)]
		  | (Unit, UnitP) => SOME []
		  | (Const x, ConstP y) => if x = y then SOME [] else NONE
		  | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
					     then all_answers match (ListPair.zip (vs, ps))
					     else NONE
					     
		  | (Constructor(s1, cv), ConstructorP(s2, cp)) => if s1 = s2
								then match(cv, cp)
								else NONE
		  | (_, _) => NONE

fun first_match value pattern_list = SOME (first_answer (fn pattern => match(value, pattern)) pattern_list)
					  handle NoAnswer => NONE
    
