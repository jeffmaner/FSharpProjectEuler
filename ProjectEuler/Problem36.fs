module Problem36

// Find the sum of all numbers less than one million, which are palindromic in base 10 and base 2.
//
// The decimal number, 585 = 1001001001 (base 2) (binary), is palindromic in both bases.
//
// Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
//
// (Please note that the palindromic number, in either base, may not include leading zeros.)


// Based on http://theburningmonk.com/2010/09/project-euler-problem-36-solution/:

open System
open System.Linq

// Checks whether the number n is palindromic in base b.
//let isPalindromic (b:int) (n:int) =
//    let cs = Convert.ToString(n, b).ToCharArray()
//    let sc = Array.rev cs
//    cs.SequenceEqual(sc)
let rec reversed k m b = if k>0 then reversed (k/b) (b*m + k%b) b else m
let isPalindromic b n = n=reversed n 0 b

let isPalB10 = isPalindromic 10
let isPalB2  = isPalindromic  2

let answer =
    [1..1000000]
    |> List.filter (fun n -> isPalB10 n && isPalB2 n)
    |> List.sum // 872187.
// Real: 00:00:01.046, CPU: 00:00:01.029, GC gen0: 76, gen1: 4, gen2: 1


// http://fsharp-euler.wikispaces.com/euler+036 doesn't use the very great libraries used above.

let isPal s =
    let len = String.length s
    let rec pal (s:string) i j =
        if not (s.[i] = s.[j]) then
           false
        elif i<j then
           pal s (i+1) (j-1)
        else
           true
    pal s 0 (len-1)

let dec2bin n =
    let rec d2b x b =
        match x with
        | 0 -> "0"     // Special case.
        | 1 -> "1" + b // End digit.
        | _ -> d2b (x/2) ((string (x%2)) + b)
    d2b n ""

let problem36 =
    {1..1000000}
    |> Seq.filter (fun x -> isPal (string x) && isPal (dec2bin x))
    |> Seq.sum // 872187.
// Real: 00:00:00.228, CPU: 00:00:00.234, GC gen0: 27, gen1: 1, gen2: 1
// Faster than the built-in libraries?! How? Why?


// Based on http://www.mathblog.dk/project-euler-36-palindromic-base-10-2/, a fascinating way to
// reverse an integer without converting it to a string and an array first. This is my attempt to
// implement a functional version of his imperative code.
// private bool IsPalindrome(int number, int b){
//   int reversed = 0;
//   int k = number;
//   while (k > 0) {
//     reversed = b * reversed + k % b;
//     k /= b;
//   }
//   return number == reversed;}
let isPalindrome b n =
    let rec reversed k m =
        if k>0 then reversed (k/b) (b*m + k%b)
        else m

    n = reversed n 0

let isPalindromeB10 = isPalindrome 10
let isPalindromeB2  = isPalindrome  2

let answer1 =
    [1..2..1000000] // Per MathBlog, any even number can't be part of the answer because it ends in zero in base 2.
    |> List.filter (fun n -> isPalindromeB10 n && isPalindromeB2 n)
    |> List.sum // 872187.
// Real: 00:00:00.135, CPU: 00:00:00.140, GC gen0: 3, gen1: 2, gen2: 0