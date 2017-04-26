module Problem43

// Find the sum of all pandigital numbers with an unusual sub-string divisibility property.
//
// The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9
// in some order, but it also has a rather interesting sub-string divisibility property.
//
// Let d[1] be the 1st digit, d[2] be the 2nd digit, and so on. In this way, we note the following:
//
//     d[2]d[3]d[4] =406 is divisible by  2
//     d[3]d[4]d[5] =063 is divisible by  3
//     d[4]d[5]d[6] =635 is divisible by  5
//     d[5]d[6]d[7] =357 is divisible by  7
//     d[6]d[7]d[8] =572 is divisible by 11
//     d[7]d[8]d[9] =728 is divisible by 13
//     d[8]d[9]d[10]=289 is divisible by 17
//
// Find the sum of all 0 to 9 pandigital numbers with this property.

let rec distribute e = function
    | []           -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | []    -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let pandigitals = permute [0..9] |> List.map (fun p -> p |> List.map string |> List.reduce (+))
// Real: 00:00:21.906, CPU: 00:00:22.807, GC gen0: 665, gen1: 243, gen2: 5
let primes = [2;3;5;7;11;13;17]
let numbers = [2..10] |> Seq.windowed 3 |> Seq.toList // [[|2; 3; 4|]; [|3; 4; 5|]; [|4; 5; 6|]; [|5; 6; 7|]; [|6; 7; 8|]; [|7; 8; 9|]; [|8; 9; 10|]]
// Real: 00:00:00.014, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

// Returns the number retrieved from taking the digits at the supplied positions.
let d ns (s:string) =
    int (ns |> Array.map (fun n -> s.[n-1].ToString()) |> Array.reduce (+))

// Identifies pandigital numbers with the desired property.
let predicate number = List.forall2 (fun n p -> (d n number) % p = 0) numbers primes

let answer = pandigitals |> List.filter predicate |> List.sumBy int64 // 16695334890L.
// Real: 00:00:02.573, CPU: 00:00:02.620, GC gen0: 662, gen1: 1, gen2: 1