module Problem50

// Which prime, below one-million, can be written as the sum of the most
// consecutive primes?
// 
// The prime 41 can be written as the sum of six consecutive primes:
// 41 = 2 + 3 + 5 + 7 + 11 + 13
// 
// This is the longest sum of consecutive primes that adds to a prime below
// one-hundred.
// 
// The longest sum of consecutive primes below one-thousand that adds to a
// prime contains 21 terms, and is equal to 953.
// 
// Which prime, below one-million, can be written as the sum of the most
// consecutive primes?

// Sieve of Eratosthenes. I didn't record the source. :(
let primes limit =
    seq{yield 2 // First prime.
        let knownComposites = new System.Collections.Generic.HashSet<int>()
        for i in 3..2..limit do // Loop through all odd numbers--evens can't be prime.
            // If it's not in our list, it's prime.
            if not (knownComposites.Contains(i)) then
               yield i

            // Add all multiples of i to our sieve, starting at i
            // and incrementing by i.
            do for j in i..i..limit do
                   knownComposites.Add(j) |> ignore}
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let mutable maxSummandsCount = 0
let mutable maxSummandsPrime = 0

let ps = primes 1000000
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

for p in ps do
    let candidateSummands = Seq.filter (fun prime -> p>prime) ps
    for i in seq{1..(Seq.length candidateSummands)} do
        let summands = Seq.windowed i candidateSummands
        for ns in summands do
            let sum = Seq.reduce (+) ns
            if sum = p then
               let summandsCount = Seq.length ns
               if summandsCount >= maxSummandsCount then
                  maxSummandsCount <- summandsCount
                  maxSummandsPrime <- p
// Ran for over four hours before I killed it.


// From http://theburningmonk.com/2010/09/project-euler-problem-50-solution/:

let max = 10000 // JM: This seems arbitrary to me.
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let mutable primeNumbers = [2]
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let hasDivisor n =
    primeNumbers
    |> Seq.takeWhile (fun n' -> n' <= int(sqrt(double n)))
    |> Seq.exists (fun n' -> n%n'=0)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let potentialPrimes = Seq.unfold (fun n -> if n>max then None else Some(n, n+2)) 3
// Real: 00:00:00.007, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

for n in potentialPrimes do
    if not (hasDivisor n) then primeNumbers <- primeNumbers @ [n]
// Real: 00:00:00.039, CPU: 00:00:00.015, GC gen0: 8, gen1: 1, gen2: 0

let isPrime n = if n=1 then false else not (hasDivisor n)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

// Compute the sum of all consecutive primes starting from n, then return the
// longest sequence which sums to a prime.
let primeSequence n =
    primeNumbers
    |> Seq.filter    (fun n' -> n'>n)
    |> Seq.scan      (fun (sum, count) n' -> (sum+n', count+1)) (n, 1)
    |> Seq.takeWhile (fun (sum, count)    -> sum<1000000)
    |> Seq.filter    (fun (sum, count)    -> isPrime sum)
    |> Seq.maxBy     (fun (sum, count)    -> count)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let answer = primeNumbers |> Seq.map primeSequence |> Seq.maxBy (fun (sum, count) -> count)
// Real: 00:00:00.493, CPU: 00:00:00.499, GC gen0: 29, gen1: 1, gen2: 0
// (997651, 543)


// From http://fsharp-euler.wikispaces.com/euler+050:

let isPrime2 n =
    if n<=1 then false
    else let m = int (sqrt (float n))
         {2..m} |> Seq.forall (fun i->n%i<>0)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let problem50 =
    let primes = {1..1000000} |> Seq.filter isPrime2 |> Seq.toArray
    let n = Array.length primes
    let sum = Array.create (n+1) 0
    for i=1 to n do
        sum.[i] <- sum.[i-1] + primes.[i-1]

    let f = Array.create 1000001 0
    let rec count i j =
        if j>0 && (sum.[i] - sum.[j])<1000000 then
           let d = sum.[i] - sum.[j]
           if f.[d]<i-j then
              f.[d]<-i-j
           count i (j-1)
    for i=1 to n do
        count i (i-1)
    let ans = primes |> Seq.map (fun i->(f.[i],i)) |> Seq.max
    ans // (543, 997651)
// Real: 00:00:06.264, CPU: 00:00:06.255, GC gen0: 58, gen1: 2, gen2: 1

// It's unusual that this solution is so much slower than the previous one.