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
let rec split = function
    | []         -> ([],[])
    | [f]        -> let (x,y) = split([])
                    (f::x, y)
    | f::s::tail -> let (x,y) = split(tail)
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

//ex4.17
let rec p q = function
    | [] -> []
    | x::xs -> let ys = p q xs
               if q x then x::ys else ys@[x]

//ex

let invert b =
    match b with
    | true  -> false
    | false -> true

let add a b =
    a + b

let first a b = 
    a

let test = function
| [] -> []
| x::xs -> xs@[x]

// sort
let rec merge = function
    | ([],[])                   -> []
    | (x::xs,[])                -> x::merge(xs,[])
    | ([],y::ys)                -> y::merge([],ys)
    | (x::xs,y::ys) when x <= y -> x::merge(xs,y::ys)
    | (x::xs,y::ys) when x > y  -> y::merge(x::xs,ys)
    | _                         -> failwith("wrong input")

let rec sort = function
| []  -> []
| [x] -> [x]
| l   -> let (l1,l2) = split l
         let s1 = sort l1
         let s2 = sort l2
         merge (s1,s2)

let randomList n range = let rand = let gen = System.Random()
                                    (fun max -> gen.Next(max))
                         List.init n (fun _ -> rand range);;

[<EntryPoint>]
let main argv =
    // [1;4;9;12;2;3;4;5;10;13]
    let xs = randomList 3000 1000000
    let m = sort xs
    printfn "%A" m
    0 // return an integer exit code
