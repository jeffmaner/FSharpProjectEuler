module Problem21

// Evaluate the sum of all amicable pairs under 10000.
//
// Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
// If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
//
// For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
//
// Evaluate the sum of all the amicable numbers under 10000.


// I can barely wrap my head around this problem, so I'm going straight to my sources...

// From http://theburningmonk.com/2010/09/project-euler-problem-21-solution/:

open System

let findDivisors n =
    let upperBound = int32 (Math.Sqrt(double n))

    [1..upperBound]
    |> Seq.filter  (fun x -> n%x = 0)
    |> Seq.collect (fun x -> [x; n/x])
    |> Seq.filter  (fun x -> x<>n)

let d n = findDivisors n |> Seq.sum

let dList = [for n=1 to 9999 do yield (n, d n)]

let answer =
    dList
    |> List.filter (fun (a, da) -> dList
                                   |> List.exists (fun (b, db) -> b = da && a = db && a<>b))
    |> List.sumBy (fun (n, dn) -> n) // 31626.
// For dList and answer:
// Real: 00:00:00.590, CPU: 00:00:00.592, GC gen0: 16, gen1: 1, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+021:

let problem21L = 
    let factSum x =
        [1..(x-1)] |> List.filter (fun y -> x%y=0) |> List.sum

    [1..10000] |> List.filter (fun x -> (factSum x)<>x && factSum(factSum x)=x) |> List.sum // 31626.
// Real: 00:00:13.668, CPU: 00:00:13.665, GC gen0: 2226, gen1: 267, gen2: 1

let problem21S =
    let factSum x =
        {1..(x-1)} |> Seq.filter (fun y -> x%y=0) |> Seq.sum

    {1..10000} |> Seq.filter (fun x -> (factSum x)<>x && factSum(factSum x)=x) |> Seq.sum // 31626.
// The Seq version is faster than the List version.
// Real: 00:00:10.496, CPU: 00:00:10.483, GC gen0: 3, gen1: 0, gen2: 0

// Why calculate factSum x twice?
let problem21Sb =
    let factSum x =
        {1..(x-1)} |> Seq.filter (fun y -> x%y=0) |> Seq.sum

    {1..10000} |> Seq.filter (fun x -> let factsumx = factSum x in
                                       factsumx<>x && (factSum factsumx)=x) |> Seq.sum // 31626.
// Real: 00:00:06.561, CPU: 00:00:06.552, GC gen0: 2, gen1: 0, gen2: 0