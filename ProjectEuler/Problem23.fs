module Problem23

// Find the sum of all the positive integers which cannot be written as the sum of
// two abundant numbers.
//
// A perfect number is a number for which the sum of its proper divisors is exactly
// equal to the number. For example, the sum of the proper divisors of 28 would be
// 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
//
// A number n is called deficient if the sum of its proper divisors is less than
// n and it is called abundant if this sum exceeds n.
//
// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
// number that can be written as the sum of two abundant numbers is 24. By
// mathematical analysis, it can be shown that all integers greater than 28123
// can be written as the sum of two abundant numbers. However, this upper limit
// cannot be reduced any further by analysis even though it is known that the
// greatest number that cannot be expressed as the sum of two abundant numbers
// is less than this limit.
//
// Find the sum of all the positive integers which cannot be written as the sum
// of two abundant numbers.

// From http://theburningmonk.com/2010/09/project-euler-problem-23-solution/:

let limit = 28123

let findDivisors n =
    let upperBound = int32 (sqrt (double n))

    [1..upperBound]
    |> Seq.filter (fun x -> n%x = 0)
    |> Seq.collect (fun x -> [x; n/x])
    |> Seq.filter (fun x -> x<>n)
    |> Seq.distinct

let isAbundant n = (findDivisors n |> Seq.sum) > n

let abundantNumbers =
    Seq.unfold (fun x -> if x<limit then Some(x, x+1) else None) 1
    |> Seq.filter isAbundant
    |> Seq.toList
// Real: 00:00:00.714, CPU: 00:00:00.717, GC gen0: 90, gen1: 1, gen2: 0

let abundantNumbersSums =
    abundantNumbers
    |> Seq.collect (fun n -> abundantNumbers |> List.map (fun m -> n+m))
    |> Seq.filter (fun n -> n <= limit)
    |> Seq.distinct
    |> Seq.toList
// Real: 00:00:12.489, CPU: 00:00:12.480, GC gen0: 605, gen1: 176, gen2: 0

let answer = ([1..limit] |> List.sum) - (abundantNumbersSums |> List.sum) // 4179871.
// Real: 00:00:00.005, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+023:

open System

let problem23 =
    let limit = 28123
    let factorSum x =
        {1..x/2} |> Seq.filter (fun i->x%i=0) |> Seq.sum
    let isAbundant x = x < (factorSum x)
    let abunds = {1..limit} |> Seq.filter isAbundant |> Seq.toArray
    let inAbunds x = Array.BinarySearch(abunds, x) >= 0
    let sumable x =
        abunds |> Seq.exists (fun a -> inAbunds (x-a))
    {1..limit} |> Seq.filter (fun x -> not (sumable x)) |> Seq.sum
// Real: 00:00:17.038, CPU: 00:00:16.941, GC gen0: 4, gen1: 2, gen2: 0