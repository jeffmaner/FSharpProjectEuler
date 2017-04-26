module Problem16

// What is the sum of the digits of the number 2^1000?
//
// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
//
// What is the sum of the digits of the number 2^1000?


// Based on http://theburningmonk.com/2010/09/project-euler-problem-16-solution/:

let number = 2I**1000
let answer = string number |> Seq.map (string >> int) |> Seq.sum // 1366.


// From http://fsharp-euler.wikispaces.com/euler+016:

let digitSum (f:int->int) (n:string) =
    n |> Seq.map (fun x -> ((int x) - 48) |> f) |> Seq.sum

let problem16 =
    let num = BigNum.PowN(2N,1000)
    digitSum (fun x->x) (num.ToString())

// Won't compile. And seems unnecessarily complex, no?