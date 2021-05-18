fun filter (P, []) = nil
|   filter (P, x::xs) = if P x then x::filter(P, xs) else filter(P, xs);

fun greater(L) = filter(fn x=> x>=hd L, L);
fun lesser(L) = filter(fn x=> x<hd L, L);

val testlist = [5,2,3,1,6,4,7,8,30];

greater(testlist);
lesser(testlist);

fun qs2 nil = nil
|   qs2 (x::xs) =  qs2(filter(fn a=> a<x, xs)) @ [x] @ qs2 (filter(fn a=> a>=x, xs));

qs2(testlist);

fun qs nil = nil
|   qs (x::xs) = 
    let
        val LT = filter(fn a=> a<x, xs)
        val GT = filter(fn a=> a>=x, xs)
    in
        qs LT @ [x] @ qs GT
    end;
    
qs [4,2,3,1,7,3,5,3,6];

fun qs2 nil = nil
|   qs2 (x::xs) =  qs2(filter(fn x=> x<hd L, L)@ [x] @ qs2 (filter(fn x=> x<hd L, L));
quickSort5 [4,2,3,1,7,3,5,3,6];