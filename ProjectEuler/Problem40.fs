module Problem40

// Finding the nth digit of the fractional part of the irrational number.
//
// An irrational decimal fraction is created by concatenating the positive integers:
//
// 0.12345678910[1]112131415161718192021...
//
// It can be seen that the 12th digit of the fractional part is 1.
//
// If d[n] represents the nth digit of the fractional part, find the value of the following expression.
//
// d[1] × d[10] × d[100] × d[1000] × d[10000] × d[100000] × d[1000000]


// From http://theburningmonk.com/2010/09/project-euler-problem-40-solution/:

let naturals = Seq.unfold (fun x -> Some(x, x+1)) 1
// Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let fraction = naturals |> Seq.collect (string >> Seq.map string)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let d n = int (fraction |> Seq.nth (n-1))
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let answer = [0..6] |> Seq.map (fun n -> d (pown 10 n)) |> Seq.reduce (*) // 210.
// Real: 00:00:00.373, CPU: 00:00:00.343, GC gen0: 32, gen1: 1, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+040:

let problem40 =
    let s = {1..1000000} |> Seq.map string |> String.concat ""
    let l = [s.[0];s.[9];s.[99];s.[999];s.[9999];s.[99999];s.[999999]]
    l |> List.map (fun x -> (int x) - (int '0')) |> List.fold (*) 1
// Real: 00:00:00.584, CPU: 00:00:00.639, GC gen0: 8, gen1: 5, gen2: 1