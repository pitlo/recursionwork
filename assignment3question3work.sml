

fun sMap (F, nil) = nil
| sMap(F,x::xs) = F x::sMap(F,xs);fun sMap (F, nil) = nil
| sMap(F,x::xs) = F x::sMap(F,xs);

fun filter (P, nil) = nil
|   filter (P, x::xs) = if P x then x::filter(P, xs) else filter(P, xs);

fun cfFilter(L) = filter(fn x => ord(hd(explode(hd L))) > 96 andalso ord(hd(explode(hd L))) < 123, L);

fun cf(L) = sMap(fn x => implode(chr(ord(hd(explode(x)))-32)::tl(explode(x))), cfFilter(fn x => ord(hd(explode(hd L))) > 96 andalso ord(hd(explode(hd L))) < 123, L);

fun cf(L) = if null L then nil
else if ((ord(hd(explode(hd L))) > 96) andalso (ord(hd(explode(hd L))) < 123)) then sMap(fn x => implode(chr(ord(hd(explode(x)))-32)::tl(explode(x))),L) else cf(tl L);

val L2 = [#"a", #"b", #"c", #"d"];
val inputlist = ["ab", "CD", " ", "12"];

sMap(fn x=>chr(ord(x)+(ord(#"A")-ord(#"a"))), L2);

(* this capializes a char: chr(ord(x)+(ord(#"A")-ord(#"a"))) *)
(*
explode the x, capitalize the first if it's uncapitalized, implode the x *)

fun cf(L) = sMap(fn x => implode(chr(ord(hd(explode(hd L)))-32)::tl(explode(hd L))) ,L);


ord(#"a");
ord(#"z");

fun testword (L) = if (chr(ord(hd(explode(hd L)))) > 96 andalso chr(ord(hd(explode(hd L)))) < 123) then true;


fun cf(L) = if null L then nil
else if (chr(ord(hd(explode(hd L)))) > 96 andalso chr(ord(hd(explode(hd L)))) < 123) then 
(sMap(fn x=> chr(ord(x)+(ord(#"A")-ord(#"a"))),[hd(explode(hd L))]) :: cf(tl L))
else hd L :: cf(tl L);

cf(inputlist);

val test = "test";
hd(explode(hd inputlist));
implode([#"J",#"R"]);