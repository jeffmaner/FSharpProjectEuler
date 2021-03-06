﻿module Problem45

// After 40755, what is the next triangle number that is also pentagonal and hexagonal?
// 
// Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
// Triangle       T(n)=n(n+1)/2       1, 3,  6, 10, 15, ...
// Pentagonal     P(n)=n(3n−1)/2      1, 5, 12, 22, 35, ...
// Hexagonal      H(n)=n(2n−1)        1, 6, 15, 28, 45, ...
// 
// It can be verified that T(285) = P(165) = H(143) = 40755.
// 
// Find the next triangle number that is also pentagonal and hexagonal.


// Based on http://theburningmonk.com/2010/09/project-euler-problem-45-solution/:
// This code is pretty, but look how long it takes! It's run over several hours now. I'm killing it.
// Hmm. His code used BigIntegers. I originally coded this not to use BigIntegers. I wonder if it will
// terminate if I use BigIntegers. Let's see.
// Well, six minutes, but that's better than not terminating.

let naturals n = Seq.unfold (fun state -> Some(state, state+1I)) n

let T n = n*(n+1I)/2I    // Triangular numbers.
let P n = n*(3I*n-1I)/2I // Pentagonal numbers.
let H n = n*(2I*n-1I)    // Hexagonal numbers.

// Sequences for each function from the point at which the brief left off.
let TSeq = naturals 285I |> Seq.map T
let PSeq = naturals 165I |> Seq.map P
let HSeq = naturals 143I |> Seq.map H

let answer =
    HSeq
    |> Seq.skip 1
    |> Seq.filter (fun h -> PSeq |> Seq.takeWhile (fun p -> p <= h) |> Seq.exists (fun p -> p = h))
    |> Seq.filter (fun h -> TSeq |> Seq.takeWhile (fun t -> t <= h) |> Seq.exists (fun t -> t = h))
    |> Seq.head // 1533776805.
// Real: 00:05:47.437, CPU: 00:05:48.662, GC gen0: 27994, gen1: 42, gen2: 4


// Based on http://fsharp-euler.wikispaces.com/euler+045:

let problem45 =
    let hex = 145 |> Seq.unfold (fun x -> Some(H(x), x+2)) // Start from the next hexagonal number and generate hexagonal numbers.
    let isPentagonal n =
        let k = (sqrt (float n * 24. + 1.) + 1.) / 6. // This is the inverse of P().
        abs (k - (float (int k))) < 1e-5 // Does the inverse of P() result in an integer?
    Seq.find isPentagonal hex // 1533776805.
// Real: 00:00:00.030, CPU: 00:00:00.046, GC gen0: 1, gen1: 0, gen2: 0 (original run, before I limited it to odd numbers)
// We can ignore triangular numbers because all triangular numbers based on odd ns are hexagonal numbers.
// That's pointed out at http://www.mathblog.dk/project-euler-45-next-triangle-pentagonal-hexagonal-number/.
// Doesn't that mean that we could start at 145 instead of 144, and increment by 2 instead of 1?
// Yes it does! And that reduces the run-time significantly!
// Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 1, gen1: 0, gen2: 0