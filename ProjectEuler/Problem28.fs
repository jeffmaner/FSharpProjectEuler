module Problem28

// What is the sum of both diagonals in a 1001 by 1001 spiral?
//
// Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed
// as follows:
//
// [21] 22 23 24 [25]
// 20  [7]  8  [9] 10
// 19  6  [1]  2 11
// 18  [5]  4  [3] 12
// [17] 16 15 14 [13]
//
// It can be verified that the sum of the numbers on the diagonals is 101.
//
// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?


// Wow. From http://theburningmonk.com/2010/09/project-euler-problem-28-solution/:

let rec sumDiagonals n m total max =
    match n with
    | 1            -> sumDiagonals (n+1) m 1 1
    | _ when n>m   -> total
    | _ when n%2=0 -> sumDiagonals (n+1) m total max
    | _            -> let newValues = [1..4] |> List.map (fun x -> max + x * (n-1))
                      let newMax    = newValues |> List.max
                      let newTotal  = total + (newValues |> List.sum)
                      sumDiagonals (n+1) m newTotal newMax

let answer = sumDiagonals 1 1001 0 0 // 669171001.
// Real: 00:00:00.033, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+028:

let problem28 =
    let n = 1001
    let rec collect depth start acc =
        if depth > n/2 then
           acc
        else
           collect (depth+1) (start+8*depth) (acc+4*start+20*depth)
    collect 1 1 1 // 669171001.
// Real: 00:00:00.000, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0


// Haskell:
// problem_28 = sum (map (\n -> 4*(n-2)^2+10*(n-1)) [3,5..1001]) + 1
let problem_28 = List.sum (List.map (fun n -> 4*(pown (n-2) 2) + 10*(n-1)) [3..2..1001]) + 1 // 669171001.
//Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

//euler28 n = sum $ scanl (+) 0
//            (1:(concatMap (replicate 4) [2,4..(n-1)]))
let euler28 n = List.sum (List.scan (+) 0 (1::(List.collect (fun lst -> List.replicate 4 lst) [2..2..(n-1)])))
euler28 1001 |> printfn "%d" // 669171001.
// Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0


// Hmm. Interesting discussion at http://www.mathblog.dk/project-euler-28-sum-diagonals-spiral/.
// The sum of the diagonals can be written as
// f(n) = 4(2n+1)^2 - 12n + f(n-1)
// ...
// Fitting a polynomial...
// ...
// f(n) = 16/3x^3 + 10x^2 + 26/3x + 1
// So f(500) = 16/3*500^3 + 10*500^2 + 26/3*500 + 1 = 669,171,001. Wow! Great discussion.