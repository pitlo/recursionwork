(* 1 *)
(* FUNCTION NAME: product(a,b) *)
(* DESCRIPTION: This function multiplies a and b and returns the result *)

fun prodneg(a,0) = 0
| prodneg(a,b) = ~a + prodneg(a,b+1);

fun product(a,0) = 0
|  product(a,b) = if b < 0 then prodneg(a,b) else a + product(a,b-1);

product(2,~3);

(* 2 *)
(* FUNCTION NAME: delnthc(s,n) *)
(* DESCRIPTION: delnthc() deletes the nth character from an inputted string.  The bulk of the work is done inside the function delnthclist.
delnthc takes in a string and explodes it into a list of chars and passes that list into delnthclist along with n, representing the position
of the char that will be deleted. *)

fun delnthclist(a::bs,1) = bs
| delnthclist(a::bs,c) = a::delnthclist(bs,c-1);

fun delnthc(s,n) = implode(delnthclist(explode(s),n));

delnthc("abcdef", 4);

(* 3 *)
(* FUNCTION NAME: dispnthc(s,n) *)
(* DESCRIPTION: dispnthc() takes a string and an int as an argument and returns the corresponding char in the string.
The function dispnthcstring(L,n) does the majority of the work.  dispnthc(s,n) passes an exploded string into dispnthcstring(L,n)
for manipulation.  *)

fun dispnthcstring(a::bs,1) = a
| dispnthcstring(a::bs,c) = dispnthcstring(bs,c-1);

fun dispnthc(s, n) = dispnthcstring(explode(s),n);

dispnthc("abcdef", 4);

(* 4 *)
(* FUNCTION NAME: multin(L) *)
(* DESCRIPTION: this function takes a list of three ints.  It multiples the first int by the second int a number of times determined
by the third int.    It utilizes a power function to help with the math and an append function to put the list in the proper
direction without utilizing @.  Example: (2,3,5) would multiply 2*3 five times and return [2,6,18,54,162,486] *)

fun pow(a,0) = 1
|  pow(a,b) = a*pow(a,b-1);

fun app([], c) = c
|  app(a::bs,c) = a::app(bs,c);

fun multin(a::b::c::nil) = if c=0 then a::nil
else app(multin(a::b::c-1::nil),[a*pow(b,c)]);

multin [2,3,5];

(* 5 *)
(* FUNCTION NAME: remv(item,L) *)
(* DESCRIPTION: This function outputs a list without any occurrences of the variable item from L.  It takes two arguments, an item and a list of variables of the same type.*)
fun remv(a,[]) = nil
| remv(a,b::cs) = if a=b then remv(a,cs) else b :: remv(a,cs);

remv("a", ["a", "b", "a", "c"]);

(* 6 *)
(* FUNCTION NAME: remvdub(L) *)
(* DESCRIPTION: This function inputs a string and outputs that string with any duplicate characters removed maintaining the first occurence of the
duplicates.  Example: remvdub(["a", "b", "a", "c", "b", "a"]) ? ["a", "b", "c"]*)

(*
fun remvdub(L) =
    if null L then nil
else if null (tl L) then hd L :: nil
else hd L :: remvdub(remv(hd L, tl L));
*)
fun remvdub([]) = nil
| remvdub(a::[]) = a::nil
| remvdub(a::bs) = remvdub(remv(a,bs));

remvdub(["a", "b", "a", "c", "b", "a"]);

(* DOESN'T WORK DOESN'T WORK DOESN'T WORK DOESN'T WORK DOESN'T WORK DOESN'T WORK DOESN'T WORK DOESN'T WORK DOESN'T WORK *)


(* 7 *)
(* FUNCTION NAME: min2(L) *)
(* DESCRIPTION: This function takes a list of ints and outputs the second smallest int in the list. *)

fun min2(a::b::[]) = if a > b then a else b
| min2(a::b::c::ds) = if a>b andalso a>c then min2(b::c::ds)
    else if b>a andalso b>c then min2(a::c::ds)
    else min2(a::b::ds);

min2([1,3,2,5,4]);

(* 8 *)
(* FUNCTION NAME: int2str(n) *)
(* DESCRIPTION: This function takes an int input and outputs that int as a string. EXAMPLE: 13 -> "13" or ~120 -> "~120" *)

(*NOT COMPLETE*)
(*NOT COMPLETE*)
(*NOT COMPLETE*)
(*NOT COMPLETE*)

(* 9 *)
(* FUNCTION NAME: str2int(s) *)
(* DESCRIPTION: This function takes a string input and outputs that string as an int. It uses a power of 10
function to help with the math and a separate function to deal with the string starting with a ~ and
consequently being a negative number. EXAMPLE: "13" -> 13 or "~120" -> ~120  *)

fun pow10(0) = 1
| pow10(a) = 10 * pow10(a-1);

fun str2listneg([],c) = 0
| str2listneg(a::bs,c) = ~(ord(a)-48) * pow10(c) + str2listneg(bs,c-1);

fun str2list([],c) = 0
| str2list(a::bs,c) = if ord(a) = 126 then str2listneg(bs,c-1)
else (ord(a)-48) * pow10(c) + str2list(bs,c-1);

fun str2int (s) =  str2list(explode(s), (size(s)-1));
    
str2int ("123456");

str2int("~123456");

(* 10 *)
(* FUNCTION NAME: pairStar(s) *)
(* DESCRIPTION: This function takes a string and outputs a string where every two consecutive equal characters are separated
by an asterisk.  The work is done primarily in pairStarL(L) while the pairStar(s) function explodes the inputted string,
runs it through pairStarL(L) and then implodes the resulting char list.*)

fun pairStarL([]) = nil
| pairStarL(a::[]) = a::nil
| pairStarL(a::bs) = if a=hd bs then a:: #"*" :: pairStarL(bs)
else a::pairStarL(bs);

fun pairStar(s) = implode(pairStarL(explode(s)));

pairStar("aaaaa wuvvxxyyy");

