module Problem1

// Problem 1:
// Add all the natural numbers below one thousand that are multiples of 3 or 5.
//
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6, and 9.
// The sum of these multiples is 23.
//
// Find the sum of all the multiples of 3 or 5 below 1000.

let multipleOf3Or5 n = (n % 3 = 0) || (n % 5 = 0)

// Test multipleOf3Or5.
//multipleOf3Or5 3 && multipleOf3Or5 5 // true.
//multipleOf3Or5 4 || multipleOf3Or5 7 // false.

let nums = [1..999]

printfn "%d" (List.sum (List.filter multipleOf3Or5 nums)) // 233168.

[1..999] |> List.filter multipleOf3Or5 |> List.sum |> printfn "%d" // 233168.

[1..999] |> List.filter (fun n -> (n%3=0)||(n%5=0)) |> List.sum |> printfn "%d" // 233168.

[1..999] |> List.sumBy (fun n -> if (n%3=0)||(n%5=0) then n else 0) |> printfn "%d" // 233168.

// Ooh, I like this:
// From http://diditwith.net/CategoryView,category,Project%2BEuler.aspx:

let inline (/=) x y = x % y = 0 // Is x evenly divisible by y?
// Well, the web page uses /:, but that's deprecated because :s are reserved for
// future use. /= makes sense in this situation, but I don't like the fact that
// it's used in Haskell for <>. I much prefer /= or != to <>. :( But if we use
// it here,

// The web page has
// [ for x in 1 .. 999 when x /= 3 or x /= 5 -> x ] |> List.sum
// but I've never seen "when" used like this; and, indeed, the compiler complains.

// This far more verbose version works.
[ for x in 1..999 do if x /= 3 || x /= 5 then yield x else yield 0 ] |> List.sum |> printfn "%d"