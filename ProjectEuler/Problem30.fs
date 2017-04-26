module Problem30

// Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
//
// Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
//
//    1634 = 1^4 + 6^4 + 3^4 + 4^4
//    8208 = 8^4 + 2^4 + 0^4 + 8^4
//    9474 = 9^4 + 4^4 + 7^4 + 4^4
//
// As 1 = 1^4 is not a sum it is not included.
//
// The sum of these numbers is 1634 + 8208 + 9474 = 19316.
//
// Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.


// From http://theburningmonk.com/2010/09/project-euler-problem-30-solution/:

// Returns the digits of a number in an array.
let digits n = n.ToString().ToCharArray() |> Array.map (fun c -> bigint.Parse(c.ToString()))

// Returns the sum of a number's digits to the specified power.
let digitsToPowerSum n pow =
    digits n |> Array.map (fun x -> pown x pow) |> Array.sum

// Returns the max sum achievable by a number of the given number of digits to the specified power.
let upperBound (n:int) pow = (bigint n) * pown 9I pow

// Finds the last number of digits n, where the max sum achievable by the digits of a number of
// n digits to the specified power is greater than the smallest number of n digits.
// Any number with more than n digits does not need to be checked.
let digitsToCheck pow =
    let n =
        Seq.unfold (fun state -> Some(state, state+1)) 1
        |> Seq.filter (fun x -> (upperBound x pow).ToString().ToCharArray().Length < x)
        |> Seq.head
    n-1

// Returns the next number with the given number of digits.
let maxNumber n = [1..n] |> List.map (fun x -> 9I * pown 10I (x-1)) |> List.sum

let answer =
    let max = maxNumber (digitsToCheck 5)
    [2I..max] |> List.filter (fun n -> n = digitsToPowerSum n 5) |> List.sum // 443839.
// Real: 00:00:05.781, CPU: 00:00:05.818, GC gen0: 528, gen1: 12, gen2: 1


// From http://fsharp-euler.wikispaces.com/euler+030:

let digitSum (f:int->int) n =
    n |> string |> Seq.map (fun x -> ((int x) - 48) |> f) |> Seq.sum

let problem30 =
    {2..1000000}
    |> Seq.map (fun x->(x,x))
    |> Seq.map (fun (x,y) -> (x, digitSum (fun x->x*x*x*x*x) y)) // x*x*x*x*x turns out to be faster than Math.Pow (what about pown?) per http://www.mathblog.dk/project-euler-30-sum-numbers-that-can-be-written-as-the-sum-fifth-powers-digits/.
    |> Seq.filter (fun (x,y) -> x=y)
    |> Seq.map (fun (x,_) -> x)
    |> Seq.sum // 443839.
// Real: 00:00:01.423, CPU: 00:00:01.435, GC gen0: 113, gen1: 0, gen2: 0


// Per http://www.mathblog.dk/project-euler-30-sum-numbers-that-can-be-written-as-the-sum-fifth-powers-digits/,
// the upper limit is 354,294.
// "We need to find a number x*(9^5) which gives us an x digit number. We can do this by hand. Since
// 9^5 = 59,049, we need at least 5 digits. 5*(9^5) = 295,245, so with 5 digits we can make a 6-digit
// number. 6*(9^5) = 354,294. So 355,000 seems like a reasonable upper bound to use." Interesting.