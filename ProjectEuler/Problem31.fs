module Problem31

// Investigating combinations of English currency denominations.
//
// In England the currency is made up of pound, £, and pence, p, and there are eight coins in general
// circulation:
//
//    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
//
// It is possible to make £2 in the following way:
//
//    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
//
// How many different ways can £2 be made using any number of coins?


// From http://theburningmonk.com/2010/10/project-euler-problem-31-solution/:

let total, coins = 200, [1;2;5;10;20;50;100;200]

// See http://www.algorithmist.com/index.php/Coin_Change.
let rec count n m =
    if n = 0 then 1
    elif n<0 then 0
    elif m<=0 && n>=1 then 0
    else (count n (m-1)) + (count (n-coins.[m-1]) m)

let answer = count total coins.Length // 73682.
// Real: 00:00:00.044, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+031:

let problem31c =
    let rec generate coins left acc = seq {
        match coins with
        | [] -> yield acc
        | x::xs -> for i=0 to left/x do
                       yield! (generate xs (left - x*i) (acc@[x])) }
    generate [200;100;50;20;10;5;2] 200 [] |> Seq.length // 73682.
// Real: 00:00:00.090, CPU: 00:00:00.093, GC gen0: 10, gen1: 0, gen2: 0