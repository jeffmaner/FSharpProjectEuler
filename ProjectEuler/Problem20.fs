module Problem20

// Find the sum of digits in 100!
//
// n! means n × (n − 1) × ... × 3 × 2 × 1
//
// For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
// and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
//
// Find the sum of the digits in the number 100!


// Combining insights from both http://theburningmonk.com/2010/09/project-euler-problem-20-solution/ and
// http://fsharp-euler.wikispaces.com/euler+020...

let factorial n = [1I..n] |> List.reduce (*)
let n = factorial 100I
let ds = string n |> Seq.map (string >> int)
ds |> Seq.sum |> printfn "%d" // 648.