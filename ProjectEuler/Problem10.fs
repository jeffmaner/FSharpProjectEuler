module Problem10

// Calculate the sum of all the primes below two million.
// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
// 
// Find the sum of all the primes below two million.

// I'm not even going to try my naive implementation of factors!
// I'll use code based on theburningmonk.com's implementation of previous
// problems.

let factors n =
    let m = int (sqrt (float n)) in
    [2..m] |> Seq.filter (fun x -> n % x = 0)

let isPrime n = factors n |> Seq.length = 0

let primeNumbers = Seq.unfold (fun x -> Some (x,x+1)) 2 |> Seq.filter isPrime

let p = primeNumbers |> Seq.takeWhile (fun p -> p < 2000000) |> Seq.sum // Ooh, Arithetic operation resulted in overflow. :(


// From http://theburningmonk.com/2010/09/project-euler-problem-10-solution/:

let factors (n:int64) =
    let m = int64 (sqrt (float n)) in
    [2L..m] |> Seq.filter (fun x -> n % x = 0L)

let isPrime (n:int64) = factors n |> Seq.length = 0

let primeSequence max = seq {for n in 2L..max do if isPrime n then yield n}
let sum = primeSequence 1999999L |> Seq.sum // 142913828922L. But it took forever.


// Let's see how fast this one runs.
// From http://fsharp-euler.wikispaces.com/euler+010:
let isPrime n =
    if n<=1 then false
    else let m = int (sqrt (float n))
         {2..m} |> Seq.exists (fun i -> n%i = 0) |> not

let problem10 =
    {1..2000000} |> Seq.filter isPrime |> Seq.map int64 |> Seq.sum // Still takes a while, but not nearly as long: 142913828922L.


// Sieve of Eratosthenes in Haskell, then in F#.
// This is not the best Haskell does--it's just where I can still follow the amazing discussion given at
// http://www.haskell.org/haskellwiki/Prime_numbers.

//-- ordered lists, difference and union
//minus (x:xs) (y:ys) = case (compare x y) of 
//           LT -> x : minus  xs  (y:ys)
//           EQ ->     minus  xs     ys 
//           GT ->     minus (x:xs)  ys
//minus  xs     _     = xs
//union (x:xs) (y:ys) = case (compare x y) of 
//           LT -> x : union  xs  (y:ys)
//           EQ -> x : union  xs     ys 
//           GT -> y : union (x:xs)  ys
//union  xs     []    = xs
//union  []     ys    = ys

// Ordered lists, difference and union.
let rec minus = function
    | ((x::xs),(y::ys)) -> match ((x::xs),(y::ys)) with
                           | _ when x<y -> minus (xs,(y::ys))
                           | _ when x=y -> minus (xs,ys)
                           | _ when x>y -> minus ((x::xs),ys)
                           | (_,y::ys)  -> y::ys
                           | (x::xs,_)  -> x::xs
                           | ([],[])    -> []
    | (_,y::ys)  -> y::ys
    | (x::xs,_)  -> x::xs
    | ([],[])    -> []

let rec union (x::xs) (y::ys) =
    match ((x::xs),(y::ys)) with
    | _ when x<y -> union xs (y::ys)
    | _ when x=y -> union xs ys
    | _ when x>y -> union (x::xs) ys
    | (_,y::ys)  -> y::ys
    | (x::xs,_)  -> x::xs
    | (_,_)      -> []

//primesToQ m = 2 : sieve [3,5..m]  where
//    sieve []     = []
//    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..m])
let primesToQ m =
    let rec sieve = function
        | [] -> []
        | (p::xs)  -> p :: sieve (minus (xs, [p*p..(p*p+2*p) - p*p..m]))
    2 :: sieve [3..2..m]

let primes = primesToQ 2000000 // [2; 3; 1999997; 1999999]. :)
// Real: 00:00:00.662, CPU: 00:00:00.717, GC gen0: 33, gen1: 6, gen2: 1
// I think it's doing all the work right, just not reporting the right results...


// From http://blogs.msdn.com/b/chrsmith/archive/2008/04/29/sieve-of-eratosthenes-in-f.aspx:

open System
open System.Collections.Generic

let primesUnderOneMillion =
    seq{yield 2 // First prime.
        let knownComposites = new HashSet<int>()
        for i in 3..2..int 1E6 do // Loop through all odd numbers--evens can't be prime.
            // If it's not in our list, it's prime.
            if not (knownComposites.Contains(i)) then
               yield i

            // Add all multiples of i to our sieve, starting at i
            // and incrementing by i.
            do for j in i..i..int 1E6 do
                   knownComposites.Add(j) |> ignore}
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0