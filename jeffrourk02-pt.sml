(* Programmer: Jeff Rourk
   Date: 10.30.2020 *)

(* 1 *)
(* FUNCTION NAME: d2b *)
(* DESCRIPTION: This function converts a non-zero number to binary.  It utilizes a appList function for appending.
This function will be utilized by several subsequent functions. *)

fun appList(L1,[]) = L1
  | appList([],L2) = L2
  | appList(x::xs,L2) = x::(appList(xs,L2));

fun convertDecimalToBinary(n) = 
if n <= 0 then nil
else appList(convertDecimalToBinary(n div 2) , [n mod 2]);

fun list2char(nil) = nil
| list2char(a::bs) = chr(a + 48) :: list2char(bs);

fun d2b(n) = implode(list2char(convertDecimalToBinary(n)));

(* 2 *)
(* FUNCTION NAME: inde *)
(* DESCRIPTION: This function returns the index (start from 1) of the occurrence of a given value.  It will return
multiple values if the given value occurs multiple times in the list*)

fun inde2(n,nil,c) = nil
| inde2(n,a::bs,c) = if n=a then c::inde2(n,bs,c+1) else inde2(n,bs,c+1);

fun inde(n,L) = inde2(n,L,1);

(* 3 *)
(* FUNCTION NAME: nele *)
(* DESCRIPTION: This function takes a list and a number n.  It returns a list with each original element repeated n times. *)

fun neleHelper(nil,c,i) = nil
| neleHelper(x::xs,c,i) = if i > c then neleHelper(xs,c,1)
else x :: neleHelper(x::xs,c,i+1);

fun nele(nil,c) = nil
| nele(L,c) = neleHelper(L,c,1);

(* 4 *)
(* FUNCTION NAME: isfact *)
(* DESCRIPTION: This function determines if a positive integer is a factorial number. It does not use any formulas. *)

fun isfactHelper(a,total,i) = 
if total > a then false
else if total=a then true
else isfactHelper(a,total*i,i+1);

fun isfact(0) = true
| isfact(a) = isfactHelper(a,1,1);

(* 5 *)
(* FUNCTION NAME: ntrin *)
(* DESCRIPTION: This function generates a list of n triangular numbers from 1. *)

fun ntrinHelper(1) = 1
| ntrinHelper(a) = (ntrinHelper(a-1) + a);

fun ntrin(0) = nil
| ntrin(a) = appList(ntrin(a-1),[ntrinHelper(a)]);

(* 6 *)
(* FUNCTION NAME: diff2 *)
(* DESCRIPTION: This function returns a list that contains the (set) difference of the two given lists. *)

fun diff2Helper(a, nil) = false
| diff2Helper(a, b::cs) = if b = a then true
else diff2Helper(a, cs);

fun diff2(nil, L) = nil
| diff2(L, nil) =  nil
| diff2(a::bs, L) = if not(diff2Helper(a, L)) then a::diff2(bs, L)
else diff2(bs, L);

(* 7 *)
(* FUNCTION NAME: occr *)
(* DESCRIPTION: This function displays the occurrence of an element of a list, or nil if the list is empty.*)

fun scrub(a,nil) = nil
| scrub(a,b::cs) = if a=b then scrub(a,cs)
else b::scrub(a,cs);

fun count(a,nil) = 0
| count(a,b::cs) = if a=b then 1+count(a,cs)
else count(a,cs);

fun occr(nil) = nil
| occr(a::bs) = (a, count(a,a::bs)) :: occr(scrub(a,a::bs));

(* 8 *)
(* FUNCTION NAME: insfront1 *)
(* DESCRIPTION: This is a higher order function that inserts an element as the head of each element of a list. *)

fun simpleMap(_, _, nil) = nil
  | simpleMap(f,n,x::xs) = f(n,x)::simpleMap(f,n,xs);

fun insfront1(n,L) = 
  simpleMap(fn(a,b) => a::b,n,L);

(* 9 *)
(* FUNCTION NAME: insfront2 *)
(* DESCRIPTION: This is equivalent to the previous function, insfront1, without the use of a higher order function. *)

fun listadder(a,L) = a::L;

fun insfront2(a,nil) = nil
|insfront2(a,b::cs) = listadder(a,b) :: insfront2(a,cs);

(* 10 *)
(* FUNCTION NAME: inseach *)
(* DESCRIPTION: This function inserts an element to each position of a list. It does not use built in functions so it recreates the length function.*)

fun lList(nil) = 0
| lList(a::bs) = 1+lList(bs);

fun placeOne(i,1,L) = i::L
| placeOne(i,p,a::bs) = a::placeOne(i,p-1,bs);

fun placeList(i,0,L) = nil
| placeList(i,l,L) = appList(placeList(i,l-1,L),[placeOne(i,l,L)]);

fun inseach(i,L) =
placeList(i,(lList(L)+1),L);

(* TEST CASES *)
d2b(7);
inde(1, [1,2,1,1,2,2,1]);
nele([1,2,3],4);
isfact(120);
ntrin(7);
diff2([1,2,3], [2,3,4]);
occr([1,2,1,2,3,2]);
insfront1(1,[[1,2],nil,[3]]);
insfront2(1,[[1,2],nil,[3]]);
inseach(4, [1,2,3]);