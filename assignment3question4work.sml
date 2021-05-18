(* FUNCTION NAME:  pTriangleH                      *)
(* DESCRIPTION:   builds a level pascal's triangle*)
fun pTriangleH(n,numer,denom) = 
    if (denom = 1) then 1 :: pTriangleH(n,numer,denom+1)
    else if (n = numer) then n :: pTriangleH(n,numer -1, denom)
    else if (numer = 1) then [1]
    else ((n*numer)div denom)::pTriangleH(((n*numer)div denom), numer-1, denom +1);
    
(* FUNCTION NAME:  pTadder                      *)
(* DESCRIPTION:   helper for pTrianlge, stacks levels of pascal's triangle*)
fun pTadder(n,x) =
    if x = 1 then [1, 1] :: pTadder(n, 2)
    else if x = 0 then [1] :: pTadder(n, 1)
    else if x = n then nil
    else if x > 1 then pTriangleH(x,x,1) :: pTadder(n, x+1)
    else nil;

(* FUNCTION NAME:  pTriangle                      *)
(* DESCRIPTION:    prints out a list of lists of pascal triangle up to level n*)
fun pt(n) = pTadder(n,0);

pt 3; 
pt 5;