(* 1 *)
(* FUNCTION NAME: pl *)
(* DESCRIPTION: This function generates a list all prime numbers up to the given number.  It uses a few helper functions, numberList to determine the range to test divisibility.  isPrime to boolean check that is utilized in the filter function.*)

fun numberList(start, finish) = if start > finish then [] else start :: numberList(start+1, finish);
numberList(2,7);

fun isPrime(n,L) = if null L then true
else if n mod hd L = 0 then false else isPrime(n,tl L);

fun filter (P, nil) = nil
|   filter (P, x::xs) = if P x then x::filter(P, xs) else filter(P, xs);

fun plHelper(L) = filter(fn x=> isPrime(x,numberList(2,x-1)), L);

fun pl(n) = plHelper(numberList(2,n));

pl(25);

(* 2 *)
(* FUNCTION NAME: qs *)
(* DESCRIPTION: This function is a quicksort in both SML and haskell preferred styles.  It uses a filter function to work out both the greater and lesser chunks and sorts them. *)

val testlist = [5,2,3,1,6,4,7,8,7];

(*SML PREFERRED *)

fun qs nil = nil
|   qs (x::xs) = 
    let
        val LT = filter(fn a=> a<x, xs)
        val GT = filter(fn a=> a>=x, xs)
    in
        qs LT @ [x] @ qs GT
    end;
    
qs testlist;

(* Haskell Preferred *)
fun qs2 nil = nil
|   qs2 (x::xs) =  qs2(filter(fn a=> a<x, xs)) @ [x] @ qs2 (filter(fn a=> a>=x, xs));

qs2 testlist;

(* 3 *)
(* FUNCTION NAME: cf *)
(* DESCRIPTION: This capitalizes each letter in a string.  I tried to get the function to only capitalize lower case letters but couldn't figure it out.
It consequently "capitalizes" everything, producing incorrect results on non capital letters *)

fun sMap (F, nil) = nil
| sMap(F,x::xs) = F x::sMap(F,xs);

fun cfFilter(L) = filter(fn x => ord(hd(explode(hd L))) > 96 andalso ord(hd(explode(hd L))) < 123, L);

fun cf(L) = sMap(fn x => implode(chr(ord(hd(explode(x)))-32)::tl(explode(x))), cfFilter(L));

cf ["ab", "CD", " ", "12"];

(* 4 *)
(* FUNCTION NAME: pt *)
(* DESCRIPTION: This is a ML function pt(n) (n>0) which return a list of lists of integers representing Pascal's triangle entries up to level n.  It utilizes two helper functions to build each level and then compile the levels into a list. *)

fun ptLevel(n,top,bot) = if (bot= 1) then 1 :: ptLevel(n,top,bot+1)
else if (n = top) then n :: ptLevel(n,top-1, bot)
else if (top= 1) then [1]
else ((n*top)div bot)::ptLevel(((n*top)div bot), top-1, bot+1);
    
fun ptHelper(n,x) = if x = 1 then [1, 1] :: ptHelper(n, 2)
else if x = 0 then [1] :: ptHelper(n, 1)
else if x = n then nil
else if x > 1 then ptLevel(x,x,1) :: ptHelper(n, x+1)
else nil;

fun pt(n) = ptHelper(n,0);

pt 3; 
pt 5;

