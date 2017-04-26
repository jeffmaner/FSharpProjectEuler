module Problem35

// How many circular primes are there below one million?
//
// The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
//
// There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
//
// How many circular primes are there below one million?


// My solution!
let factors n =
    [1..n] |> List.filter (fun x -> n%x=0)

let isPrime1 n = (factors n).Length = 2 // (1 and n only).

let isCircularPrime1 n =
    let rec f m =
        let ds = m.ToString().ToCharArray()
        let x = System.Int32.Parse(System.String.Join("", (Array.append ds.[1..ds.Length-1] [| ds.[0] |])))
        if x = n then
           true // We've rotated through the entire number.
        elif isPrime1 x then
           f x
        else
           false
    f n

let myAnswer = [1..1000000] |> List.filter isPrime1 |> List.filter isCircularPrime1 |> List.length
// I let it run through lunch, then killed it! :)


// From http://theburningmonk.com/2010/09/project-euler-problem-35-solution/:

let hasDivisor n =
    let upperBound = int (sqrt (double n))
    [2..upperBound] |> Seq.exists (fun x -> n%x=0)

let isPrime n = if n<2 then false else not (hasDivisor n)

let rotate n =
    let cs = n.ToString().ToCharArray() |> Array.toList
    let len = List.length cs
    [0..(len-1)]
    |> List.map (fun r -> List.permute (fun i -> (i+r) % len) cs)
    |> List.map (fun l -> System.String.Join("", l |> List.toArray))
    |> List.map int

let isCircularPrime n = rotate n |> List.forall isPrime

let answer = [2..999999] |> Seq.filter isPrime |> Seq.filter isCircularPrime |> Seq.length // 55.
// Real: 00:01:18.556, CPU: 00:01:18.468, GC gen0: 16041, gen1: 12, gen2: 1


// http://fsharp-euler.wikispaces.com/euler+035:
let problem35 =
    let isCircular x =
        let rec isc x i n =
            if isPrime x then
               if i=n then true
               else isc (int (string (x%10) + string (x/10))) (i+1) n
            else
               false
        isc x 1 (String.length (string x))
    {1..1000000} |> Seq.filter isCircular |> Seq.length // 55.
// Real: 00:01:12.540, CPU: 00:01:12.384, GC gen0: 14867, gen1: 7, gen2: 0


// Let's try utilizing the Sieve of Eratosthenes.

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

let ps = primes 1000000
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let isPrime n = ps |> Seq.exists (fun p -> p=n)

// And let's just check odd numbers, because even numbers can't be prime.
let answer2 = [3..2..999999] |> Seq.filter isPrime |> Seq.filter isCircularPrime |> Seq.length // 55.
// I don't understand why this isn't faster than it answer above. In fact, it's eternal. What?
let answer3 = ps |> Seq.filter isCircularPrime |> Seq.length
// This takes forever, too. What is going on?