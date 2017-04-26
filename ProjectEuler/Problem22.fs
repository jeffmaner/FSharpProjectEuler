module Problem22

// What is the total of all the name scores in the file of first names?
//
// Using names.txt, a 46K text file containing over five-thousand first names,
// begin by sorting it into alphabetical order. Then working out the alphabetical
// value for each name, multiply this value by its alphabetical position in the
// list to obtain a name score.
//
// For example, when the list is sorted into alphabetical order, COLIN, which is
// worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
// obtain a score of 938 × 53 = 49714.
//
// What is the total of all the name scores in the file?

open System.IO

let path = @"C:\Users\TGHG\Documents\Visual Studio 2010\Projects\ProjectEuler\ProjectEuler\names.txt"


// From http://theburningmonk.com/2010/09/project-euler-problem-22-solution/:

let values =
    File.ReadAllLines(path)
    |> Array.map (fun s -> s.Replace("\"", ""))
    |> Array.collect (fun s -> s.Trim().Split(','))
    |> Array.sort
    |> Array.map (fun s -> s.ToUpper().ToCharArray() |> Array.sumBy (fun c -> (int32 c)-(int32 'A' - 1)))

let answer = Array.map2 (fun v p -> v*p) values [|1..values.Length|] |> Array.sum // 871198282.
// Real: 00:00:00.006, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+022:

let stringSum s =
    s |> Seq.sumBy (fun x -> (int x) - 64)

let problem22 =
    let names = let text = File.ReadAllText path in text.Replace("\"","").Split ','
    names
    |> Seq.sort
    |> Seq.mapi (fun i n -> (stringSum n) * (i+1))
    |> Seq.sum // 871198282.
// Real: 00:00:00.034, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0
