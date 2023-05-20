(*Week 4 - Extra Practice Problems*)

fun compose_opt f g x =
    case g x of
	NONE => NONE
      | SOME y => case f y of
		      NONE => NONE
		    | SOME z => z
				    
    
fun do_until f g x =
    let val y = f x
    in case g y of
	   true => do_until f g y
	 | false => y
    end


val factorial = do_until (fn x => 
