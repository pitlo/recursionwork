(* 1 *)
(* FUNCTION NAME: product(a,b) *)
(* DESCRIPTION: This function multiplies a and b and returns the result. It utilizes a second function to handle cases where b is negative. *)

fun prodneg(a,b) = 
if b = 0 then 0
else ~a + prodneg(a,b+1);

fun product(a,b) = 
  if b = 0 then 0
else if b < 0 then prodneg(a,b)
else a + product(a,b-1);

product(2,~3);

(* 2 *)
(* FUNCTION NAME: delnthc(s,n) *)
(* DESCRIPTION: delnthc() deletes the nth character from an inputted string.  The bulk of the work is done inside the function delnthclist.
delnthc takes in a string and explodes it into a list of chars and passes that list into delnthclist along with n, representing the position
of the char that will be deleted. *)
fun delnthclist(L,n) =
    if n=1 then tl L
    else hd L:: delnthclist(tl L, n-1);

fun delnthc(s,n) = implode(delnthclist(explode(s),n));

delnthc("abcdef", 4);

(* 3 *)
(* FUNCTION NAME: dispnthc(s,n) *)
(* DESCRIPTION: dispnthc() takes a string and an int as an argument and returns the corresponding char in the string.
The function dispnthcstring(L,n) does the majority of the work.  dispnthc(s,n) passes an exploded string into dispnthcstring(L,n)
for manipulation.  *)
fun dispnthcstring(L,n) =
   if n=1 then hd L
   else dispnthcstring(tl L, n-1);

fun dispnthc(s, n) = dispnthcstring(explode(s),n);

dispnthc("abcdef", 4);

(* 4 *)
(* FUNCTION NAME: multin(L) *)
(* DESCRIPTION: this function takes a list of three ints.  It multiples the first int by the second int a number of times determined
by the third int.    It utilizes a power function to help with the math and an append function to put the list in the proper
direction without utilizing @.  Example: (2,3,5) would multiply 2*3 five times and return [2,6,18,54,162,486] *)

fun pow(n,p)=
if p=0 then 1
else n*pow(n,p-1);

fun app(one,two) =
    if null one then two
else
  hd one:: app(tl one, two);

fun multin(L) =
let
    val FIRST = hd L;
    val SECOND = hd (tl L) ;
    val THIRD = hd (tl (tl L));
in
    if THIRD  = 0 then FIRST :: nil
    else 
app(multin(FIRST::SECOND::THIRD-1::nil),[FIRST * pow(SECOND,THIRD)])
end;

multin [2,3,5];

(*NOT COMPLETE*)
(*NOT COMPLETE*)
(*NOT COMPLETE*)
(*NOT COMPLETE*)

(* 5 *)
(* FUNCTION NAME: remv(item,L) *)
(* DESCRIPTION: This function outputs a list without any occurrences of the variable item from L.  It takes two arguments, an item and a list of variables of the same type.*)
fun remv(item, L) =
 if null L then nil
 else if (hd L = item) then remv(item,tl L)
 else hd L :: remv(item, tl L);

remv("a", ["a", "b", "a", "c"]);

(* 6 *)
(* FUNCTION NAME: remvdub(L) *)
(* DESCRIPTION: This function inputs a string and outputs that string with any duplicate characters removed maintaining the first occurence of the
duplicates.  Example: remvdub(["a", "b", "a", "c", "b", "a"]) -> ["a", "b", "c"]*)

fun remvdub(L) =
    if null L then nil
else if null (tl L) then hd L :: nil
else hd L :: remvdub(remv(hd L, tl L));

remvdub(["a", "b", "a", "c", "b", "a"]);

(* 7 *)
(* FUNCTION NAME: min2(L) *)
(* DESCRIPTION: This function takes a list of ints and outputs the second smallest int in the list. *)
fun min2(L) =
if tl(tl L) = nil then
(*if there are only two items in the list, return the larger of the two *)
    if hd(L) > hd(tl L) then hd(L) else hd(tl L)
else
(* compare the first three items of the list, run the program again on the list with the largest removed *)
    if hd L > hd(tl L) andalso hd L > hd(tl(tl L)) then min2(tl L)
else
    if hd(tl L) > hd L andalso hd(tl L) > hd(tl(tl L)) then min2(hd L:: tl(tl L))
else
    min2(hd L :: hd(tl L) :: tl(tl(tl L)));

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

fun pow10 n =
    if n = 0 then 1 else 10*pow10(n-1);

fun str2listneg (L,n) =
    if null L then 0 
    else ~(ord(hd L)-48) * pow10(n) + str2listneg(tl L, n-1);

fun str2list (L,n) =
    if null L then 0
else if ord(hd L) = 126 then str2listneg(tl L, n-1)
    else (ord(hd L)-48) * pow10(n) + str2list(tl L, n-1);

fun str2int (string) =
    str2list(explode(string), (size(string)-1));
    
str2int ("123456");

str2int("~123456");

(* 10 *)
(* FUNCTION NAME: pairStar(s) *)
(* DESCRIPTION: This function takes a string and outputs a string where every two consecutive equal characters are separated
by an asterisk.  The work is done primarily in pairStarL(L) while the pairStar(s) function explodes the inputted string,
runs it through pairStarL(L) and then implodes the resulting char list.*)

fun pairStarL(L) =
    if null L then nil
    else if null (tl L) then hd L :: nil
    else if hd L = hd(tl L) then hd L :: #"*" :: pairStarL(tl L)
    else hd L :: pairStarL(tl L);

fun pairStar(s) = implode(pairStarL(explode(s)));

pairStar("aaaaa wuvvxxyyy");
