module Problem32

// Find the sum of all numbers that can be written as pandigital products.
//
// We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n
// exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
//
// The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand,
// multiplier, and product is 1 through 9 pandigital.
//
// Find the sum of all products whose multiplicand/multiplier/product identity can be written
// as a 1 through 9 pandigital.
// HINT: Some products can be obtained in more than one way so be sure to only include it once
// in your sum.


// From http://theburningmonk.com/2010/09/project-euler-problem-32-solution/:
//
// I don't follow his discussion of the limits from 1000 to 9999, but he gives a justification.

let isPandigital n =
    let upperBound = int (sqrt (double n))

    [2..upperBound]
    |> Seq.filter (fun x -> n%x = 0)
    |> Seq.map (fun x -> x.ToString() + (n/x).ToString() + n.ToString())
    |> Seq.exists (fun s -> [1..9]
                            |> List.map (fun n -> n.ToString())
                            |> List.forall (fun n -> s.Contains(n) && s.IndexOf(n) = s.LastIndexOf(n)))

let answer = [1000..9999] |> List.filter isPandigital |> List.sum // 45228, and pretty quick!
// Real: 00:00:00.305, CPU: 00:00:00.249, GC gen0: 30, gen1: 0, gen2: 0


// I'm not crazy about the solution at http://fsharp-euler.wikispaces.com/euler+032.