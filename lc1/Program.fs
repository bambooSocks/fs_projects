// Learn more about F# at http://fsharp.org

open System

// ex1
let rec f n = 
    match n with
    | 0             -> 0
    | n when n >= 0 -> n + f(n-1)
    | _             -> failwith "No negative numbers"

// ex2
let rec sum (m,n) = 
    match n with
    | 0                     -> m
    | n when n > 0 && m > 0 -> (m + n) + sum(m, n-1)
    | _                     -> failwith "No negative numbers"

// ex3
let rec bin (n,k) = 
    match (n,k) with
    | (n', 0)               -> 1
    | (n', k') when n' = k' -> 1
    | (n', k') when n' > k' -> bin(n'-1, k'-1) + bin(n-1, k)
        
// ex4
let rec multiplicity (x, ys) =
    match ys with
    | []               -> 0
    | h::t when h = x  -> 1 + multiplicity(x,t)
    | h::t when h <> x -> multiplicity(x,t)

// ex5
let rec mulC (x, ys) =
    match ys with
    | []   -> []
    | h::t -> h*x::mulC(x,t)

// ex6
let rec addE (xs, ys) =
    match (xs, ys) with
    | ([],[])         -> []
    | (hx::tx,[])     -> hx::addE(tx,[])
    | ([],hy::ty)     -> hy::addE([],ty)
    | (hx::tx,hy::ty) -> hx+hy::addE(tx,ty)

// ex7a
let mulX (poly) = 0::poly

// ex7b
let rec mul (p1, p2) =
    match p1 with
    | []   -> []
    | h::t -> addE(mulC(h,p2),mulX(mul(t,p2)))

// ex7c
let printPoly (p) =
    match p with
    | [] -> sprintfn ""
    | h::t -> sprintf "%d x^%d" h t.Length + printPoly(t)

[<EntryPoint>]
let main argv =
    // let m = f(-4)
    let m = mul([2; 3; 0; 1], [1; 2; 3; 0])
    printfn "%A" m

    printPoly([2;5;4;6;0;5])
    0 // return an integer exit code
