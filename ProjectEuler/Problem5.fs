module Problem5

// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

// First, based on http://theburningmonk.com/2010/09/project-euler-problem-5-solution/:

let inline (%?) x y = x % y = 0
let inline divides x y = x %? y

let allDivide ns n = ns |> Seq.forall (divides n)

// Smallest Common Multiple.
let scm numbers =
    let max = Array.max numbers
     in Seq.unfold (fun x -> Some (x, x+1)) 1
        |> Seq.map ((*) max)
        |> Seq.filter (allDivide numbers)
        |> Seq.head

let commonMultiplier = scm [|1..20|] // 232792560.

// Next, from http://fsharp-euler.wikispaces.com/euler+005:

let rec gcd x y = if y=0L then x else gcd y (x%y)
let lcm x y = x*y / (gcd x y)
{1L..20L} |> Seq.fold lcm 1L |> printfn "%d" // 232792560.

// This implementation is much faster!
// The problem asks to get the least common multiplier of 1 to 20. We use a
// property lcm(a1, a2, ..., an) = lcm(a1, lcm(a2, ..., an) = ..., which reduces
// the problem to solving the lcm of two numbers incrementally.