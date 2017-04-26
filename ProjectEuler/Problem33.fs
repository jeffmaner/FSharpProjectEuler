module Problem33

// Discover all the fractions with an unorthodox cancelling method.
//
// The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it
// may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
//
// We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
//
// There are exactly four non-trivial examples of this type of fraction, less than one in value, and
// containing two digits in the numerator and denominator.
//
// If the product of these four fractions is given in its lowest common terms, find the value of the
// denominator.


// I don't like the code at The Burning Monk, or my other source for functional solutions--they're just
// too much code.

// The Haskell solution is much simpler, but uses list comprehensions and a few definitions I'm
// unfamiliar with.

// So I'll go with the imperative solution offered at
// http://www.mathblog.dk/project-euler-33-fractions-unorthodox-cancelling-method/. He also has a nice
// discussion about reducing the problem space.



// The imperative solution at http://www.mathblog.dk/project-euler-33-fractions-unorthodox-cancelling-method/:

let problem33 =
    let rec gcd x y = if y=0 then x else gcd y (x%y)

    let mutable denProduct = 1
    let mutable nomProduct = 1

    for i = 1 to 9 do
        for den = 1 to i-1 do
            for nom = 1 to den-1 do
                if (nom*10+i)*den = nom*(i*10+den) then
                   denProduct <- denProduct * den
                   nomProduct <- nomProduct * nom
    denProduct <- denProduct / (gcd nomProduct denProduct)

    denProduct // 100.
// Real: 00:00:00.000, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

// My attempt to get rid of the fors. Not very pretty, or even correct!
{1..9}
|> Seq.map (fun i -> {1..i-1}
                     |> Seq.map (fun d -> {1..d-1}
                                          |> Seq.map (fun n -> Seq.fold (fun (dP,nP) x -> if (n*10+i)*d = n*(i*10+d) then
                                                                                             (dP*d,nP*n)
                                                                                          else (dP,nP)) (1,1)))) |> Seq.collect (fun x -> x) |> Seq.filter (fun (dP,nP) -> dP<>1 && nP<>1) |> (fun (dP,nP) -> dP/(gcd nP dP)) |> printfn "%d"

{1..9}
|> Seq.iter (fun i -> {1..i-1}
                     |> Seq.iter (fun d -> {1..d-1}
                                          |> Seq.iter (fun n -> printfn "(%d,%d,%d)" i d n))) |> ignore