module Problem15

// Starting in the top left corner in a 20 by 20 grid, how many routes are there
// to the bottom right corner?


// From http://theburningmonk.com/2010/09/project-euler-problem-15-solution/:

let rec factorial (n:bigint) = if n<=1I then 1I else n*factorial (n-1I)
let combo n k = factorial n / (factorial k * factorial (n-k))
let answer = combo 40I 20I // 137846528820.

// A comment on this page states:
let p15 =
    let factorial n = [1I..n] |> List.reduce (*)
    factorial (2I*20I)/(factorial 20I ** 2) // 137846528820.


// From http://fsharp-euler.wikispaces.com/euler+015:

let problem15 =
    let rec bigFact = function
                      | 0 | 1 -> 1N
                      | n     -> Math.bignum.FromInt(n) * bigFact (n-1)
    bigFact 40 / bigFact 20 / bigFact 20

// Won't compile.


// Translation from Haskell's solution (http://www.haskell.org/haskellwiki/Euler_problems/11_to_20):
// problem_15 = product [21..40] `div` product [2..20]
let (product : bigint list -> bigint) = List.reduce (*)
product [21I..40I] / product [2I..20I] |> printfn "%A" // 137846528820.