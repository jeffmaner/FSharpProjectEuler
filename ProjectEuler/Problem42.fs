module Problem42

// How many triangle words does the list of common English words contain?
//
// The nth term of the sequence of triangle numbers is given by, t[n] = ½n(n+1); so the first ten triangle
// numbers are:
//
// 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
//
// By converting each letter in a word to a number corresponding to its alphabetical position and adding
// these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t[10].
// If the word value is a triangle number then we shall call the word a triangle word.
//
// Using Problem42.txt, a 16K text file containing nearly two-thousand common English words, how many are
// triangle words?

// I tried starting off on my own, and almost immediately hit a trivial problem. So I gave up, and just
// started implementing a solution based on http://theburningmonk.com/2010/09/project-euler-problem-42-solution/.

open System.IO

let path = @"C:\Users\TGHG\Documents\Visual Studio 2010\Projects\ProjectEuler\ProjectEuler\Problem42.txt"

let words = let text = File.ReadAllText path in text.Replace("\"", "").Split ','
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let wordValue (w:string) =
    w
    |> Seq.map (fun c -> int c - 64) // 64 = int 'A' - 1.
    |> Seq.sum

printfn "%d" (wordValue "SKY") // 55.
// Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let wordValues = words |> Seq.map wordValue
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
let maxWordValue = wordValues |> Seq.max // 192.
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let naturals = Seq.unfold (fun state -> Some(state, state+1)) 1
let T n = n*(n+1)/2
let TSeq = naturals |> Seq.map T

let answer =
    TSeq
    |> Seq.takeWhile (fun t -> t <= maxWordValue)
    |> Seq.map (fun t -> wordValues |> Seq.filter ((=) t) |> Seq.length)
    |> Seq.sum // 162.
// Real: 00:00:00.038, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let longestWordLength = words |> Array.map (fun s -> s.Length) |> Array.max