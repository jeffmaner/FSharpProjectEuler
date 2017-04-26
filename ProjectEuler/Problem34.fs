module Problem34

// Find the sum of all numbers which are equal to the sum of the factorial of their digits.
//
// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
//
// Find the sum of all numbers which are equal to the sum of the factorial of their digits.
//
// Note: as 1! = 1 and 2! = 2 are not sums they are not included.


// From http://fsharp-euler.wikispaces.com/euler+034:

let problem34 =
    let rec fact = function
        | 0 | 1 -> 1
        | n     -> n*fact(n-1)

    let digitSum (f:int->int) n =
        n |> string |> Seq.map (fun x -> ((int x) - 48) |> f) |> Seq.sum // 48 is Asc("0").

    {1..3265920} |> Seq.map (fun x -> (x, digitSum fact x)) |> Seq.filter (fun (x,y) -> x=y) // Where did 3265920 come from?
    |> Seq.map (fun (x,y) -> x) |> Seq.sum // 40733. N.b. The answer is 40730.
// Real: 00:00:04.665, CPU: 00:00:04.664, GC gen0: 338, gen1: 2, gen2: 1


// From http://theburningmonk.com/2010/09/project-euler-problem-34-solution/, a lot of code.

open System.Collections.Generic

let factorial n = if n=0 then 1 else [1..n] |> List.reduce (*)

// Memoize factorials.
let factorials = new Dictionary<int,int>()                       // Could have used an array, e.g. see
do [0..9] |> List.iter (fun n -> factorials.Add(n, factorial n)) // below.
// Real: 00:00:00.009, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

// Returns the digits of n in an array.
let digits n = n.ToString().ToCharArray() |> Array.map (string >> int)

// Returns the sum of the factorials of a number's digits.
let sumOfFacsOfDigitsOfN n = digits n |> Array.map (fun d -> factorials.[d]) |> Array.sum

// Returns the max sum achievable by the number based on the number of its digits.
let upperBound n = n * (factorials.[9])

// Returns the last number of digits n, where the max sum achievable by the factorials of the
// digits of a number of n digits is greater than the smallest number of n digits.
// Any number with more than n digits does not need to be checked.
let digitsToCheck =
    let n = Seq.unfold (fun state -> Some(state, state+1)) 1
            |> Seq.filter (fun x -> (upperBound x).ToString().ToCharArray().Length < x)
            |> Seq.head
    n-1 // 7.
// Real: 00:00:00.021, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

// Returns the next number with the given number of digits.
let maxNumber ds = [1..ds] |> List.map  (fun x -> 9 * pown 10 (x-1)) |> List.sum

let answer =
    let max = maxNumber digitsToCheck // 9999999.
    [3..max] |> List.filter (fun n -> n = sumOfFacsOfDigitsOfN n) |> List.sum // 40730. But why start at 3?
// Real: 00:00:23.112, CPU: 00:00:23.618, GC gen0: 3615, gen1: 33, gen2: 3


// From http://www.mathblog.dk/project-euler-34-factorial-digits/, an imperative approach.

let problem34I =
    let fact n = if n=0 then 1 else [1..n] |> List.reduce (*)
    let facts = [| 0..9 |] |> Array.map fact
    let mutable result = 0
    for i=10 to 2540160 do // 2540160 is 9!*7. 9!*8 also results in a seven-digit number, so 9!*7 is an upper limit.
        let mutable sumOfFacts = 0
        let mutable number = i
        while number>0 do
              let d = number % 10
              number <- number / 10
              sumOfFacts <- sumOfFacts + facts.[d]
        if sumOfFacts = i then
           result <- result + i
    result // 40730.
// Real: 00:00:00.203, CPU: 00:00:00.405, GC gen0: 0, gen1: 0, gen2: 0


// Haskell solution:
// import Data.Char
// problem_34 = sum [ x | x <- [3..100000], x == facsum x ]
//     where facsum = sum . map (product . enumFromTo 1 . digitToInt) . show

let fac n = [1..n] |> List.reduce (*)
let facsum n = n.ToString() |> Seq.map (int >> fac) |> Seq.sum
[3..100000] |> List.filter (fun x -> x = facsum x) |> List.sum |> printfn "%d" // 0 :(
// Real: 00:00:02.841, CPU: 00:00:02.854, GC gen0: 552, gen1: 3, gen2: 1