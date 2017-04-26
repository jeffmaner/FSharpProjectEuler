module Problem9

// Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
//
// A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
// a^2 + b^2 = c^2
// 
// For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
//
// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
// Find the product abc.

// My usual two sources for code both used imperative code. So I'm going to
// attempt a translation from the Haskell implementation of the solution:
//
// triplets l = [[a,b,c] | m <- [2..limit],
//                         n <- [1..(m-1)], 
//                         let a = m^2 - n^2, 
//                         let b = 2*m*n, 
//                         let c = m^2 + n^2,
//                         a+b+c==l]
//     where limit = floor . sqrt . fromIntegral $ l
//  
// problem_9 = product . head . triplets $ 1000

open System

let triplets limit =
    let lim = int (Math.Floor(Math.Sqrt(float limit)))

    let m = [2..lim]   // Well, bummer. How do we implement this in f# without 
    let n = [1..(m-1)] // for do?

    let a = m^2 - n^2
    let b = 2*m*n
    let c = m^2 + n^2

    a+b+c=limit


// From http://theburningmonk.com/2010/09/project-euler-problem-9-solution/:

let isPythagoreanTriplet numbers =
    match List.sort numbers with
    | [a;b;c] -> a*a + b*b = c*c
    | _       -> false

let triplets = seq {for a=1 to 1000 do
                        for b=1 to 1000 do
                            for c=1 to 1000 do
                                if a+b+c=1000 then yield [a;b;c]}

let pythagoreanTriplet = triplets |> Seq.filter isPythagoreanTriplet |> Seq.head // [200; 375; 425].
let product = pythagoreanTriplet |> Seq.reduce (*) // 31875000.


// From http://fsharp-euler.wikispaces.com/euler+009:

let problem9 =
    seq {for a in 1..1000 do
             for b in 1..1000 do
                 let c = 1000 - a - b
                 if a*a + b*b = c*c then
                    yield a*b*c} |> Seq.max // 31875000.
// Real: 00:00:00.345, CPU: 00:00:00.343, GC gen0: 0, gen1: 0, gen2: 0
// This runs so much faster!


// Based on http://stefanoricciardi.com/2010/08/25/project-euler-problem-9-in-f/:

// Generate triplets using Euclid's formula.
let pythagoreanTriplets top =
    [for m in 1..top do
         for n in 1..m-1 do
             let a = m*m-n*n
             let b = 2*m*n
             let c = m*m+n*n
             yield [a;b;c]]

let findTripletWithSum sum =
    pythagoreanTriplets sum
    |> List.find (fun [a;b;c] -> a+b+c=sum)

let problem9a = findTripletWithSum 1000 |> List.reduce (*) // 31875000.
// Real: 00:00:00.710, CPU: 00:00:00.733, GC gen0: 12, gen1: 6, gen2: 1

