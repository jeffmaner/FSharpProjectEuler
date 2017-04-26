module Problem17

// How many letters would be needed to write all the numbers in words from 1 to 1000?
//
// If the numbers 1 to 5 are written out in words: one, two, three, four, five,
// then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
//
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out
// in words, how many letters would be used?
//
// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
// forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
// letters. The use of "and" when writing out numbers is in compliance with British
// usage.


// I'm intrigued by the answer at http://fsharp-euler.wikispaces.com/euler+017,
// which I've changed up to my tastes:

let chars x =
    match x with
    |  0                          -> 0
    |  1 |  2 |  6 | 10           -> 3
    |  4 |  5 |  9                -> 4
    |  3 |  7 |  8 | 40 | 50 | 60 -> 5
    | 11 | 12 | 20 | 30 | 80 | 90 -> 6
    | 15 | 16 | 70                -> 7
    | 13 | 14 | 18 | 19           -> 8
    | 17                          -> 9
    |  _                          -> let err = sprintf "failed with %A" x in failwith err

let rec convert n =
    match n with
    | x when x=1000 -> 3 + 8 // 3 for "one", 8 for "thousand".
    | x when x<=20  -> chars x
    | x when x<100  -> let (t,o) = (x/10*10, x%10)
                        in chars t + chars o
    | _             -> let h = n/100
                        in if n%100=0
                           then chars h + 7 // 7 for "hundred".
                           else chars h + 7 + 3 + convert (n%100) // 7 for "hundred", 3 for "and".

let problem17 =
    {1..1000} |> Seq.map convert |> Seq.sum // 21124.


// The burning monk's solution was more intuitive, but was a lot more code.
// http://theburningmonk.com/2010/09/project-euler-problem-17-solution/.