module Problem3

// Problem 3:
// Find the largest prime factor of a composite number.
//
// The prime factors of 13195 are 5, 7, 13 and 29.
//
// What is the largest prime factor of the number 600851475143?

let factorP n m = n%m = 0L

let factors (n:int64) =
    let rec f c a =
        if c<n then
           if factorP n c then
              f (c+1L) (c::a)
           else
              f (c+1L) a
        else
           List.rev a
    f 1L []

let primeP (n:int64) = (n>1L) && (List.length (factors n) = 1)

let primeFactors (n:int64) = factors n |> List.filter primeP

let fs = factors 600851475143L     // This takes an eternity to run.
let pfs = fs |> List.filter primeP // So I never get here,
let mpf = pfs |> List.max          // or here.

primeFactors 600851475143L |> List.max |> printfn "%d" // Eternity.


// From http://fsharp-euler.wikispaces.com/euler+003:
let problem3 =
    let num = 600851475143L
    let rec findMax (n:int64) (i:int64) =
        if n=i || n<i then
            n
        elif n%i=0L then
            findMax (n/i) i
        else
            findMax n (i+1L)
    findMax num 2L // 6857.
// !


let inline (%?) x y = x%y=0L
let inline divides x y = x %? y
let sqrt = System.Math.Sqrt
let bound = double >> sqrt >> int64
// Based on http://theburningmonk.com/2010/09/project-euler-problem-3-solution/
let factors (n:int64) =
    let m = bound n // m is upper bound of search space.
     in [2L..m] |> Seq.filter (divides n)

let prime (n:int64) = factors n |> Seq.length = 0

// Maximum Prime Factor of n.
let mpf (n:int64) =
    let m = bound n // m is upper bound of search space.

    [2L..m]
    |> (Seq.filter (divides n)
    >>  Seq.filter prime
    >>  Seq.max)

let maxPrime = mpf 600851475143L // 6857.


// From http://diditwith.net/CategoryView,category,Project%2BEuler.aspx:

let primeFactors n =
  let inline isFactor n d = n % d = 0L

  let rec nextFactor n d =
    let x = if d = 2L then 3L else d+2L
    if isFactor n x then x else nextFactor n x

  let rec findFactors n d acc =
    if isFactor n d then
      findFactors (n/d) d (d::acc)
    elif n > d then
      findFactors n (nextFactor n d) acc
    else
      acc

  findFactors n 2L [] |> List.rev

let toLargest = List.fold max 1L

primeFactors 600851475143L |> toLargest |> printfn "%d"

// My attempt to implement Haskell's solution, which is, I think, the Seive of Eratosthenes.
//let rec factor n (p::ps) =
//    match p::ps with
//    | _ when p*p > n -> [n]
//    | _ when n%p = 0 -> p :: factor (n/p) (p::ps)
//    | _              -> factor n ps
//
//let rec primes n = 2 :: List.filter (primeFactors >> List.length >> (fun c -> c=1)) [3..2..System.Int32.MaxValue]
//and primeFactors n = factor n primes