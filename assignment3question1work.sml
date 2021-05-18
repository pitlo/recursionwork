fun enum (n, m) = if n > m then [] else n :: enum (n+1, m)
fun sieve [] = nil
| sieve (n::ns) = n :: sieve (List.filter (fn m => m mod n > 0) ns)
fun pl n = sieve (enum (2, n));

pl(100);

fun filter (P,nil) = nil
| filter (P,x::xs) = if P x then x::filter(P,xs) else filter(P,xs);

filter(fn x=>x>5, [4,8,1,9,7]);
fun pl(L) = filter(x) => isPrime(x), L);

fun filter (P, nil) = nil
|   filter (P, x::xs) = if P x then x::filter(P, xs) else filter(P, xs);

fun foo(L) = filter(fn x=>x>5, L);