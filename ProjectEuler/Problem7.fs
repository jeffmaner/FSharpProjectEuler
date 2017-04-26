module Problem7

// Find the 10001st prime.
// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
//
// What is the 10 001st prime number?

let factorP n m = n%m = 0

let factors n =
    let rec f c a =
        if c<n then
           if factorP n c then
              f (c+1) (c::a)
           else
              f (c+1) a
        else
           List.rev a
    f 1 []

factors 10 |> printfn "%A"
// factors works, but it's naive and terribly inefficient.

let primeP n = (n>1) && (List.length (factors n) = 1)

// Already exists! :)
//let nth n xs =
//    let rec f m ys a =
//        match m,ys with
//        | _,[] -> failwith "Empty list."
//        | _,z::zs when n=a -> z
//        | _,z::zs -> f m zs (a+1)
//    f n xs 0

Seq.unfold (fun (x) -> Some (x,x+1)) 1 |> Seq.map factors |> Seq.filter primeP |> Seq.nth 6 |> printfn "%d"
Seq.unfold (fun (x) -> Some (x,x+1)) 1 |> List.ofSeq |> nth 10002 |> printfn "%d"

// Not thinking about this right...


// From http://fsharp-euler.wikispaces.com/euler+007:

let isPrime n =
    if n<=1 then false
    else let m = int (sqrt (float n))
         {2..m} |> Seq.exists (fun i -> n%i = 0) |> not

let rec nextPrime n = if isPrime n then n
                      else nextPrime (n+1)

let problem7 =
    let primes = 2 |> Seq.unfold (fun p -> let next = nextPrime (p+1) in Some (p,next))
    Seq.nth 10000 primes // 104743.

// From http://theburningmonk.com/2010/09/project-euler-problem-7-solution/:

open System

let factors n =
    let m = int (sqrt (float n)) in
    [2..m] |> Seq.filter (fun x -> n % x = 0)

let isPrime n = factors n |> Seq.length = 0

let primeNumbers = Seq.unfold (fun x -> Some (x,x+1)) 2 |> Seq.filter isPrime

let p = primeNumbers |> Seq.nth 10000 // 104743.