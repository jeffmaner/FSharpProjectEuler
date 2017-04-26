module Problem6

// What is the difference between the sum of the squares and the square of the sums?

// The sum of the squares of the first ten natural numbers is,
// 1^2 + 2^2 + ... + 10^2 = 385
//
// The square of the sum of the first ten natural numbers is,
// (1 + 2 + ... + 10)^2 = 552 = 3025
//
// Hence the difference between the sum of the squares of the first ten natural
// numbers and the square of the sum is 3025 − 385 = 2640.
//
// Find the difference between the sum of the squares of the first one hundred
// natural numbers and the square of the sum.

let sumOfSquares ns =
    ns
    |> List.map (fun n -> n*n)
    |> List.sum
    // List.sum (List.map (fun n -> n*n) ns)

let squareOfSum ns =
    let n = List.sum ns
    n*n

// Testing:
//[1..10] |> sumOfSquares |> printfn "%d" //  385.
//[1..10] |> squareOfSum  |> printfn "%d" // 3025.
//
//[1..10] |> sumOfSquares |> (fun x -> ([1..10] |> squareOfSum) - x) |> printfn "%d" // 2640.

let numbers = [1..100]
numbers |> sumOfSquares |> (fun x -> (numbers |> squareOfSum) - x) |> printfn "%d" // 25164150.
(squareOfSum numbers) - (sumOfSquares numbers) |> printfn "%d" // 25164150.

// I like the approach at http://fsharp-euler.wikispaces.com/euler+006, mainly defining sqr.
// Although the use of Seq results in fewer keystrokes, too!
let sqr x = x*x
let s1 = {1..100} |> Seq.map sqr |> Seq.sum
let s2 = {1..100} |> Seq.sum     |> sqr
printfn "%d" (s2 - s1)