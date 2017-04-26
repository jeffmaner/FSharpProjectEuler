module Problem4

// A palindromic number reads the same both ways. The largest palindrome made
// from the product of two 2-digit numbers is 9009 = 91 × 99.
//
// Find the largest palindrome made from the product of two 3-digit numbers.

// Stolen from http://theburningmonk.com/2010/09/project-euler-problem-4-solution/!

open System.Linq

let isPalindromic n =
    let cs = n.ToString().ToCharArray()
    let sc = cs.Reverse()
    cs.SequenceEqual(sc)

let ns = [100..999]

let ps = ns |> List.collect (fun x -> ns |> List.map (fun y -> x * y))
let maxPalindromic = ps |> Seq.filter isPalindromic |> Seq.max

printfn "%d" maxPalindromic // 906609.