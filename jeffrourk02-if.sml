(* Programmer: Jeff Rourk
   Date: 10.30.2020 *)


(* 1 *)
(* FUNCTION NAME: d2b *)
(* DESCRIPTION: This function converts a non-zero number to binary.  It utilizes a appList function for appending.
This function will be utilized by several subsequent functions. *)

fun appList(L1,L2) = if null L2 then L1
else if null L1 then L2
else hd L1::appList(tl L1, L2);

fun convertDecimalToBinary(n) = 
if n <= 0 then nil
else appList(convertDecimalToBinary(n div 2) , [n mod 2]);

fun list2char(L) =
if null L then nil
else chr(hd L + 48) :: list2char(tl L);

fun d2b(n) = implode(list2char(convertDecimalToBinary(n)));

(* 2 *)
(* FUNCTION NAME: inde *)
(* DESCRIPTION: This function returns the index (start from 1) of the occurrence of a given value.  It will return
multiple values if the given value occurs multiple times in the list*)

fun inde2(n,L,c) =
if null L then nil
else if n = hd L then c :: inde2(n,tl L, c+1)
else inde2(n,tl L, c+1);

fun inde(n,L) = inde2(n,L,1);

(* 3 *)
(* FUNCTION NAME: nele *)
(* DESCRIPTION: This function takes a list and a number n.  It returns a list with each original element repeated n times. *)

fun neleHelper(L,c,i) = 
if null L then nil else
if i > c then neleHelper(tl L,c,1)
else hd L :: neleHelper(L,c,i+1);

fun nele(L,c) = 
if null L then nil 
else neleHelper(L,c,1);

(* 4 *)
(* FUNCTION NAME: isfact *)
(* DESCRIPTION: This function determines if a positive integer is a factorial number. It does not use any formulas. *)

fun isfactHelper(a,total,i) = 
if total > a then false
else if total=a then true
else isfactHelper(a,total*i,i+1);

fun isfact(a) = 
if a=0 then true 
else isfactHelper(a,1,1);

(* 5 *)
(* FUNCTION NAME: ntrin *)
(* DESCRIPTION: This function generates a list of n triangular numbers from 1. *)

fun ntrinHelper(a) = if a=1 then 1
else (ntrinHelper(a-1)+a);

fun ntrin(a) =
if a=0 then nil
else appList(ntrin(a-1),[ntrinHelper(a)]);

(* 6 *)
(* FUNCTION NAME: diff2 *)
(* DESCRIPTION: This function returns a list that contains the (set) difference of the two given lists. *)

fun diff2Helper(a, L) = if null L then false
else if hd L = a then true
else diff2Helper(a, tl L);

fun diff2(L1,L2) = if null L1 then nil
else if null L2 then nil
else if not(diff2Helper(hd L1, L2)) then hd L1::diff2(tl L1, L2)
else diff2(tl L1, L2);

(* 7 *)
(* FUNCTION NAME: occr *)
(* DESCRIPTION: This function displays the occurrence of an element of a list, or nil if the list is empty.*)

fun scrub(ele, L) =
if null L then nil
else if ele = hd L then scrub(ele, tl L)
else hd L :: scrub(ele, tl L);

fun count(ele, L) =
if null L then 0
else if ele = hd L then 1 + count(ele, tl L)
else count(ele, tl L);

fun occr(L) =
if null L then nil
else (hd L, count(hd L, L)) :: occr(scrub(hd L, L));

(* 8 *)
(* FUNCTION NAME: insfront1 *)
(* DESCRIPTION: This is a higher order function that inserts an element as the head of each element of a list. *)

fun simpleMap(f,n,L) =
if null L then nil
else f(n,hd L)::simpleMap(f,n,tl L);

fun insfront1(n,L) = 
simpleMap(fn(a,b) => a::b,n,L);

(* 9 *)
(* FUNCTION NAME: insfront2 *)
(* DESCRIPTION: This is equivalent to the previous function, insfront1, without the use of a higher order function. *)

fun listadder(ele,L) = ele::L;

fun insfront2(ele,L) =
if null L then nil
else listadder(ele,hd L) :: insfront2(ele, tl L);

(* 10 *)
(* FUNCTION NAME: inseach *)
(* DESCRIPTION: This function inserts an element to each position of a list. It does not use built in functions so it recreates the length function.*)

fun lList(L) =
if null L then 0
else 1 + lList(tl L);

fun placeOne(item,pos,L) =
if pos = 1 then item::L
else hd L::placeOne(item,pos-1,tl L);

fun placeList(item,length,L) =
if length = 0 then nil
else appList(placeList(item,length-1,L),[placeOne(item,length,L)]);

fun inseach(item,L) =
placeList(item,(lList(L)+1),L);

(* TEST CASES *)
d2b(7);
inde(1, [1,2,1,1,2,2,1]);
nele([1,2],3);
isfact(120);
ntrin(7);
diff2([1,2,3], [2,3,4]);
occr([1,2,1,2,3,2]);
insfront1(1,[[1,2],nil,[3]]);
insfront2(1,[[1,2],nil,[3]]);
inseach(4, [1,2,3]);