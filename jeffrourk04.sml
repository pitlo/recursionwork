MAP:
(defun smap (F L)
    (if (null L) NIL
        (cons (funcall F (first L)) (smap F (rest L)))))

How to use MAP:
(defun f1 (N) (+ 10 N))
(smap (function f1) '(1 3 5))
or:
(smap (function first) '((1 2) (3 4) (5 6)))


FILTER:
(defun fil(P L)
    (if (null L) NIL
        (if (funcall P (first L)) (cons (first L) (fil P (rest L)))
            (fil P (rest L)))))

How to use FILTER:
(fil (function foo) '((1 2) (4 3) (6 5)))


Write a function delnth to delete the n-th element of a list. You may assume that the input string is always longer than n.
(dispnth '(1 (2 3) 4 5) 2) ? (2 3)
Following is for a character from a string

fun delnthclist(L,n) =
    if n=1 then tl L
    else hd L:: delnthclist(tl L, n-1);


(defun deltnh (L n)
(if (equal n 1) (rest L)
(else (cons (first L) (delnth (rest L) (- n 1))))))

fun delnthc(s,n) = implode(delnthclist(explode(s),n));

CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD CRASHES HARD 




Write a function dispnth to display the n-th element of a list. You may assume that the input string is always longer than n.
(delnth '(1 2 (3 4) 5) 3) ? (1 2 5)

(defun dispnth(L n)
(if (equal n 1) (first L)
(dispnth (rest L) (- n 1))))

WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE 




Write a function remv to remove elements from a list (including all multiple appearance)
(remv a '(a (b) a c) ? ((B) C)
(* FUNCTION NAME: remv(item,L) *)
(* DESCRIPTION: This function outputs a list without any occurrences of the variable item from L.  It takes two arguments, an item and a list of variables of the same type.*)

(defun remv(i L) (if (null L) nil
(if (equal (first L) i) (remv i (rest L))
(cons (first L) (remv i (rest L))))))

WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE 





Write a function remv2 to remove given list
elements from a list (including multiple appearance)
(remv2 '(a b) '(a b (a b) c)) ? (A B C)

(defun remv2(i L) (if (null L) nil
(if (equal (first L) i) (remv i (rest L))
(cons (first L) (remv i (rest L))))))

WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE 




Write a function remvdub to remove duplicate elements from a list
(remvdub '(a b a c b a)) ? (A B C)
(* FUNCTION NAME: remvdub(L) *)
(* DESCRIPTION: This function inputs a string and outputs that string with any duplicate characters removed maintaining the first occurence of the
duplicates.  Example: remvdub(["a", "b", "a", "c", "b", "a"]) -> ["a", "b", "c"]*)

(defun remvdub (L)
(if (null L) nil
(if (equal (rest L) nil) (cons (first L) nil)
(cons (first L) (remvdub(remv (first L) (rest L)))))))

WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE 




Write a function remvdub2 to remove duplicate
elements (single element or lists) from a list.
(remvdub2 '(a b (a) c b (a))) ? (a b c (a))

(defun remvdub2 (L)
(if (null L) nil
(if (equal (rest L) nil) (cons (first L) nil)
(cons (first L) (remvdub2(remv (first L) (rest L)))))))

OUTPUTS BUT NOT CORRECT OUTPUTS BUT NOT CORRECT OUTPUTS BUT NOT CORRECT OUTPUTS BUT NOT CORRECT OUTPUTS BUT NOT CORRECT OUTPUTS BUT NOT CORRECT 




Write a function min2 to compute the second  smallest of number of an integer list. You may assume the list has at least 2 numbers and all numbers are distinct. 
(min2 '(1 3 2 5 4)) ? 2
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

fun min2(L) =
if tl(tl L) = nil then
    if hd(L) > hd(tl L) then hd(L) else hd(tl L)
else
    if hd L > hd(tl L) andalso hd L > hd(tl(tl L)) then min2(tl L)
else
    if hd(tl L) > hd L andalso hd(tl L) > hd(tl(tl L)) then min2(hd L:: tl(tl L))
else
    min2(hd L :: hd(tl L) :: tl(tl(tl L)));

(defun min2 (L) 
(cond
((and (equal (rest(rest L)) nil) (> (first L) (first(rest L)))) (first L))
((equal (rest(rest L)) nil) (first(rest L)))
((and (> (first L) (first(rest L))) (> (first L) (first(rest(rest L))))) (min2(rest L)))
((and (> (first(rest L))) (>(first(rest L)) (first(rest(restL))))) (min2(cons (first L) (rest(rest L)))))           
(t (min2 (cons (first L) (first(rest L)) (rest(rest(rest L))))))))




Write a function inde which returns the index (start from 1) of the occurrence of a given value.
(inde 1 '(1 2 1 1 2 2 1)) ? (1 3 4 7)
(* FUNCTION NAME: inde *)
(* DESCRIPTION: This function returns the index (start from 1) of the occurrence of a given value.  It will return
multiple values if the given value occurs multiple times in the list*)

(defun inde2(n L c)
(if (null L) nil
(if (equal n (first L)) (cons c (inde2 n (rest L) (+ c 1)))
(inde2 n (rest L) (+ c 1)))))

(defun inde (n L) (inde2 n L 1))

WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE 




Write a function nele which repeats each element in a list n times.
(nele '(1 3 5) 3)? (1 1 1 3 3 3 5 5 5)

(defun neleHelper(L c i)
(if (null L) nil
(if (> i c) (neleHelper (rest L) c 1)
(cons (first L) (neleHelper L c (+ i 1))))))

(defun nele (L c)
(if (null L) nil
(neleHelper L c 1)))

DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS DONE WORKS 




Write a function occr to display the occurrence of an element of a list, or nil if the list is empty.
(occr '(1 2 1 2 3 2)) ? ((1 2) (2 3) (3 1))
(* FUNCTION NAME: occr *)
(* DESCRIPTION: This function displays the occurrence of an element of a list, or nil if the list is empty.*)

fun scrub(ele, L) =
if null L then nil
else if ele = hd L then scrub(ele, tl L)
else hd L :: scrub(ele, tl L);
(defun scrub(ele L) (if (null L) NIL
(if (= ele (first L)) (scrub ele (rest L))
(cons (first L) (scrub ele (rest L))))))
THE SCRUB METHOD WORKS

fun count(ele, L) =
if null L then 0
else if ele = hd L then 1 + count(ele, tl L)
else count(ele, tl L);
(defun cou(ele L)
(if (null L) 0
(= ele (first L)) (+ 1 (cou ele (rest L)))
(cou ele (rest L))))
THE COUNT METHOD DOESN'T WORK

fun occr(L) =
if null L then nil
else (hd L, count(hd L, L)) :: occr(scrub(hd L, L));




Implement function mergesort. (let) is optional, not required.
(mergesort '(5 3 2 11 7)) ? (2 3 5 7 11)
STILL NEED THE SML IMPLEMENTATION
STILL NEED THE SML IMPLEMENTATION
STILL NEED THE SML IMPLEMENTATION
STILL NEED THE SML IMPLEMENTATION


Write a function qs(L) quick sort a list.
(qsort '(4 2 3 1 7 3 5 3 6)) ? (1 2 3 3 3 4 5 6 7)
a) qs is a higher order function with anonymous function
b) qs applies a standard (filter P L) function
c) total two functions, filter and qsort
d) (append) is allowed

fun qs nil = nil
|   qs (x::xs) = 
    let
        val LT = filter(fn a=> a<x, xs)
        val GT = filter(fn a=> a>=x, xs)
    in
        qs LT @ [x] @ qs GT
    end;

(defun filter(P L)
    (if (null L) NIL
        (if (funcall P (first L)) (cons (first L) (filter P (rest L)))
            (fil P (rest L)))))

(defun filter (P L) (if (null L) nil
(if (funcall P (first L) (first(rest L))) nil
(rest L ))))

(defun qs (L) (if (null (rest L)) nil
(append (qs(filter (lambda (a b) (< a b)) L )) (list(first L)) (qs(filter (lambda (a b) (>= a b)) L )))))

(print(qs '(1 2 4 3 6 5)))