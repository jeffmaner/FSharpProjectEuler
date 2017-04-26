module Problem48

// Find the last ten digits of 1^1 + 2^2 + ... + 1000^1000.
// 
// The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
// 
// Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

open System.Numerics

let problem48 =
    seq {1I..1000I}
    |> Seq.map (fun n -> BigInteger.Pow(n, int n))
    |> Seq.reduce (fun x y -> BigInteger.Add(x,y))
    |> string      // All this just to get the last ten digits.
    |> Seq.toList  //
    |> List.rev    //
    |> Seq.ofList  //
    |> Seq.take 10 //
    |> Seq.toList
    |> List.rev // ['9'; '1'; '1'; '0'; '8'; '4'; '6'; '7'; '0'; '0']
    // |> String.collect string // I don't understand what the problem is with this line.
// Real: 00:00:00.121, CPU: 00:00:00.109, GC gen0: 1, gen1: 1, gen2: 0