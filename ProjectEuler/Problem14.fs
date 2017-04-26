module Problem14

// Find the longest sequence using a starting number under one million.
//
// The following iterative sequence is defined for the set of positive integers:
//
// n → n/2 (n is even)
// n → 3n + 1 (n is odd)
//
// Using the rule above and starting with 13, we generate the following sequence:
// 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//
// It can be seen that this sequence (starting at 13 and finishing at 1) contains
// 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
// that all starting numbers finish at 1.
//
// Which starting number, under one million, produces the longest chain?
//
// NOTE: Once the chain starts the terms are allowed to go above one million.

// From http://theburningmonk.com/2010/09/project-euler-problem-14-solution/:

let nextNumber n = if n%2L = 0L then n/2L else 3L*n+1L

let findSequenceLength n =
    let mutable count = 1L
    let mutable current = n

    while current > 1L do
          current <- nextNumber current
          count   <- count + 1L
    count

let longestSeq = [1L..999999L] |> Seq.maxBy findSequenceLength // 837799L.

// Interesting. He offers a functional, though admittedly slower, approach:
// The sequence returned by Seq.unfold does not include 1, so for completeness sake, add 1 to the length
let findSequenceLengthF n = Seq.length(Seq.unfold (fun x -> if x = 1L then
                                                               None
                                                            else
                                                               Some(x, nextNumber x)) n) + 1 // 837799L.
// It's not much slower!


// From http://fsharp-euler.wikispaces.com/euler+014:

open System.Collections.Generic

let memoize (f: 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input: 'a) =
        match dict.TryGetValue(input) with
        | true , x -> x
        | false, _ -> let answer = f input
                      dict.Add(input, answer)
                      answer
    memoizedFunc

let problem14 =
    let rec n3 = // What the heck? n3 is a recursive value/object, not a recursive function?
        let f x = match x with // What the heck?
                  | 1L -> 1
                  | n when n%2L=1L -> 1 + (n3 (3L*n+1L))
                  | n -> 1+n3 (n/2L)
        memoize f

    {1..1000000} |> Seq.maxBy (fun n -> n3 (int64 n)) // 837799.

// It's fast! Note the warning. Don't know what that's about.
