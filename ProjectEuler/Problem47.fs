module Problem47

// Find the first four consecutive integers to have four distinct primes factors.
//
// The first two consecutive numbers to have two distinct prime factors are:
//
// 14 = 2 × 7
// 15 = 3 × 5
//
// The first three consecutive numbers to have three distinct prime factors are:
//
// 644 = 2² × 7 × 23
// 645 = 3 × 5 × 43
// 646 = 2 × 17 × 19.
//
// Find the first four consecutive integers to have four distinct primes factors.
// What is the first of these numbers?


// Thank you http://diditwith.net/CategoryView,category,Project%2BEuler.aspx for primeFactors.
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
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

// Distinct Prime Factors.
let DPFs x y =
    primeFactors y |> List.toSeq |> Seq.distinct |> Seq.length = x
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let problem47 =
    seq {1L..1000000L}
    |> Seq.windowed 4 
    |> Seq.filter (fun a -> Seq.forall (fun n -> DPFs 4 n) a)
    |> Seq.head // [|134043L; 134044L; 134045L; 134046L|]
// Real: 00:00:09.113, CPU: 00:00:09.110, GC gen0: 61, gen1: 1, gen2: 0