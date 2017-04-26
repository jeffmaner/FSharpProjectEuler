module Problem41

// What is the largest n-digit pandigital prime that exists?
//
// We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
// For example, 2143 is a 4-digit pandigital and is also prime.
//
// What is the largest n-digit pandigital prime that exists?


// From http://theburningmonk.com/2010/09/project-euler-problem-41-solution/:

let rec distribute e = function
    | []           -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | []    -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let hasDivisor n =
    let upperBound = int (sqrt (double n))
    [2..upperBound] |> Seq.exists (fun x -> n%x=0)

let isPrime n = if n < 2 then false else not (hasDivisor n)

let answer =
    [1..9]
    |> List.collect (fun m -> [1..m] |> permute)
    |> List.map (fun l -> l |> List.map string |> List.reduce (+))
    |> List.map int
    |> List.filter isPrime
    |> List.max // 7652413.
// Real: 00:14:19.503, CPU: 00:14:17.677, GC gen0: 64626, gen1: 23399, gen2: 10! That's hardly satisfactory!

// From Problem37.fs:
let eSieve limit =
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

let eSieve1 limit =
    let sieveBound = int ((limit-1)/2)
    let upperSqrt  = ((int (sqrt (double limit))) - 1)/2

    let primeBits = new System.Collections.BitArray(sieveBound+1, true)

    for i = 1 to upperSqrt do
        if primeBits.Get(i) then
           let mutable j = i*2*(i+1)
           while j<sieveBound do
                 primeBits.Set(j, false)
                 j <- j + 2*i+1

    seq {yield 2 // First prime.
         for i = 1 to sieveBound do
             if primeBits.Get(i) then
                yield 2*i+1}

let ps = eSieve1 987654320 // 987654321 will include 987654321 in the list of primes, and will be, obviously, the largest pandigital "prime".
// Real: 00:00:09.221, CPU: 00:00:09.204, GC gen0: 0, gen1: 0, gen2: 0

// From Problem38.fs:
let isPandigitalI n =
    let mutable digits = 0
    let mutable count = 0
    let mutable tmp = 0
    let mutable result = true
    let mutable m = n

    while m>0 do
          tmp <- digits
          digits <- digits ||| (1 <<< (int ((m%10) - 1)))
          if tmp = digits then
             result <- false

          count <- count + 1
          m <- m/10
    result && digits = (1 <<< count) - 1

let isPandigitalF n =
    let s = n.ToString()
    [1..s.Length]
    |> List.map string
    |> List.forall (fun x -> s.Contains(x) && s.IndexOf(x) = s.LastIndexOf(x))

// When ps was defined using eSieve, the system ran out of memory.
//let answer1 = ps |> Seq.filter isPandigitalF |> Seq.max // System.OutOfMemoryException at line 48.
//let answer2 = ps |> Seq.filter isPandigitalI |> Seq.max // System.OutOfMemoryException at line 48.

// When ps was defined by eSieve1:
let answer1 = ps |> Seq.filter isPandigitalF |> Seq.max // 7652413.
// Real: 00:06:21.413, CPU: 00:06:20.767, GC gen0: 27350, gen1: 43, gen2: 4

let answer2 = ps |> Seq.filter isPandigitalI |> Seq.max // 7652413.
// Real: 00:03:19.002, CPU: 00:03:18.698, GC gen0: 1280, gen1: 20, gen2: 2

//> isPrime 987654321;;
//Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
//val it : bool = false
//> Seq.length ps;;
//Real: 00:03:06.400, CPU: 00:03:06.062, GC gen0: 1280, gen1: 20, gen2: 2
//val it : int = 50251452
//> ps |> Seq.nth (50251452-1)
//Real: 00:03:08.350, CPU: 00:03:07.794, GC gen0: 1279, gen1: 18, gen2: 1
//val it : int = 987654321
// So eSieve1 includes its limit, whether its limit is prime or not. Hmph.

// At http://www.mathblog.dk/project-euler-41-pandigital-prime/ he notes that all pandigital numbers except
// for the four-digit and seven-digit ones are divisible by three, and are therefore not prime. So he sets
// the maximum number to check at 7654321 instead of 987654321, which significantly speeds up the discovery
// process.
//
// 1+2+3+4+5+6+7+8+9 = 45
// 1+2+3+4+5+6+7+8 = 36
// 1+2+3+4+5+6+7 = 28
// 1+2+3+4+5+6 = 21
// 1+2+3+4+5 = 15
// 1+2+3+4 = 10
// 1+2+3 = 6
// 1+2 = 3

let answer3 = eSieve1 7654320 |> Seq.filter isPandigitalF |> Seq.max // 7652413.
// Real: 00:00:03.429, CPU: 00:00:03.416, GC gen0: 226, gen1: 0, gen2: 0
let answer4 = eSieve1 7654320 |> Seq.filter isPandigitalI |> Seq.max // 7652413.
// Real: 00:00:01.804, CPU: 00:00:01.809, GC gen0: 13, gen1: 1, gen2: 0