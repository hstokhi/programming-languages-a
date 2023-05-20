(* HW 1 Assignments *)
fun is_older(d1: int*int*int, d2: int*int*int)=
    if #1 d1 = #1 d2
    then if #2 d1 = #2 d2
	 then #3 d1 < #3 d2
	 else #2 d1 < #2 d2
    else #1 d1 < #2 d2
		    
		       
(*2*)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
	if #2 (hd dates) = month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)
		 
	     


(*3*)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(*4*)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
	if #2 (hd dates) = month
	then hd dates::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)
			   
(*5 - should return a list, not a list of lists*)	
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

(*6*)							  
fun get_nth(list: string list, n: int) =
    if n = 1
    then hd list
    else get_nth(tl list, n - 1)
		
(*7*)	     
fun date_to_string(date: (int*int*int)) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", "^  Int.toString(#1 date)
    end
    
(*8*)  
fun number_before_reaching_sum(sum: int, nums: int list) =
    if null nums
    then 0
    else
	if hd nums >= sum
	then 0
	else 1 + number_before_reaching_sum(sum - hd nums, tl nums)
					   
(*9*)					   
fun what_month(day: int) =
    if day <= 0
    then 0
    else let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
	 in 1 + number_before_reaching_sum(day, months)
	 end

(*10*)
fun month_range(day1: int, day2: int) =
   if day1 < day2
   then what_month(day1)::month_range(day1 + 1, day2)
   else []
	    
(*11*)
fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
	let fun length(list: (int*int*int) list) =
		if null list
		then 0
		else 1 + length(tl list)
				     
	in
	    if length(dates) = 1
	    then SOME (hd dates)
	    else
		if is_older(hd dates, hd (tl dates))    
		then SOME (hd dates)
		else oldest(tl dates)
	end
	    
		       
		      
(*Challenge*)
fun remove_duplicates(months: int list) =
    if null months
    then []
    else
	if hd months <> hd (tl months)
	then hd months::remove_duplicates(tl months)
	else remove_duplicates(tl months)

			      
	    
	     

	    
