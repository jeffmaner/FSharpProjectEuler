module Problem49

// The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
// increases by 3330, is unusual in two ways: (i) each of the three terms are
// prime, and, (ii) each of the 4-digit numbers are permutations of one another.
//
// There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
// exhibiting this property, but there is one other 4-digit increasing sequence.
//
// What 12-digit number do you form by concatenating the three terms in this
// sequence?

let hasDivisor n =
    let upperBound = int (sqrt (double n))
    [2..upperBound] |> Seq.exists (fun x -> n%x=0)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let isPrime n = if n < 2 then false else not (hasDivisor n)
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let primes4 = seq {1488..9999} |> Seq.filter isPrime // Start just beyond the first number we already know about.
// Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let mutable (answer:int list) = []
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let arePermutations x y =
    let rec arePerms xs ys =
        match xs,ys with
        | w::ws,z::zs -> if w=z then arePerms ws zs else false
        | [],[]       -> true
        | _,[]        -> false
        | [],_        -> false

    arePerms (List.ofSeq (Seq.sort (string x))) (List.ofSeq (Seq.sort (string y)))
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let problem49 =
    for a in primes4 do
        for b in Seq.filter (fun p -> p>a) primes4 do
            if arePermutations a b then
               let c = 2*b-a
               if Seq.exists (fun p -> p=c) primes4 then
                  if arePermutations a c then
                     answer <- [a;b;c] //  [2969; 6299; 9629].
// Real: 00:03:15.611, CPU: 00:03:15.094, GC gen0: 32539, gen1: 22, gen2: 2