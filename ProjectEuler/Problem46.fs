module Problem46

// What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
// 
// It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
// 
//  9 =  7 + 2×1^2
// 15 =  7 + 2×2^2
// 21 =  3 + 2×3^2
// 25 =  7 + 2×3^2
// 27 = 19 + 2×2^2
// 33 = 31 + 2×1^2
// 
// It turns out that the conjecture was false.
// 
// What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?


// Based on http://theburningmonk.com/2010/09/project-euler-problem-46-solution/:

// The Burning Monk used int64s, but that's not necessary, so I didn't in copying his code. I also used
// function composition where he did not.

let hasDivisor n =
    let upperBound = int (sqrt (double n))
    [2..upperBound] |> Seq.exists (fun x -> n%x=0)

let isPrime n = if n < 2 then false else not (hasDivisor n)

let oddCompositeNumbers =
    Seq.unfold (fun state -> Some(state, state+2)) 9
    |> Seq.filter (isPrime >> not)

let primes = Seq.unfold (fun state -> Some(state, state+2)) 1 |> Seq.filter isPrime

let isSum n =
    primes
    |> Seq.takeWhile (fun x -> x<n)
    |> Seq.exists (fun x -> sqrt (double ((n-x)/2)) % 1.0 = 0.0)

let answer = oddCompositeNumbers |> Seq.filter (isSum >> not) |> Seq.head // 5777.
// Real: 00:00:01.669, CPU: 00:00:01.950, GC gen0: 200, gen1: 1, gen2: 1
// Actually, using ints instead of int64s might have speeded up the code just a little.


// My other F# source uses the function isPrime, but doesn't define it, and I don't feel like hunting down
// its definition.