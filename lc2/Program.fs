// Learn more about F# at http://fsharp.org

open System

//ex2.1
let f = function
    | n when (n%2=0 || n%3=0) && n%5<>0 -> true
    | _                                 -> false

//ex2.2
let rec pow (s,n) = 
    match n with
    | 0          -> ""
    | n when n>0 -> s + pow(s,n-1)
    | _          -> failwith "n is negative number"

//ex4.3
let rec evenN n = 
    match n with
    | 0 -> []
    | n -> evenN(n-1)@[n*2]
    
//ex4.8
let rec split l =
    match l with
    | []                        -> ([],[])
    | f::tail when l.Length = 1 -> let (x,y) = split(tail)
                                   (f::x, y)
    | f::s::tail                -> let (x,y) = split(tail)
                                   (f::x, s::y)

//ex4.9
let rec zip (xs,ys) =
    match (xs,ys) with
    | ([],[])       -> []
    | (x::xt,[])    -> [(x,0)] @ zip(xt,[])
    | ([],y::yt)    -> [(0,y)] @ zip([],yt)
    | (x::xt,y::yt) -> [(x,y)] @ zip(xt,yt)

//ex4.12
let predicate x = x >0

let rec sum (p,xs) = 
    match xs with
    | []              -> 0
    | x::xt when p(x) -> x + sum(p, xt)
    | x::xt           -> sum(p, xt)


[<EntryPoint>]
let main argv =
    let m = sum (predicate, [1;-5;10;0;26;-77])
    printfn "%A" m
    0 // return an integer exit code
