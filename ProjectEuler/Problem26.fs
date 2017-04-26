module Problem26

// Find the value of d < 1000 for which 1/d contains the longest recurring cycle.
//
// A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with
// denominators 2 to 10 are given:
//
//    1/2	= 	0.5
//    1/3	= 	0.(3)
//    1/4	= 	0.25
//    1/5	= 	0.2
//    1/6	= 	0.1(6)
//    1/7	= 	0.(142857)
//    1/8	= 	0.125
//    1/9	= 	0.(1)
//    1/10	= 	0.1
//
// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit
// recurring cycle.
//
// Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.


// From http://theburningmonk.com/2010/09/project-euler-problem-26-solution/:

let naturalNumbers = Seq.unfold (fun state -> Some(state, state+1)) 1

let rec cycleLength n =
    if n % 2I = 0I then cycleLength (n/2I)
    elif n % 5I = 0I then cycleLength (n/5I)
    else naturalNumbers
         |> Seq.filter (fun x -> ((pown 10I x) - 1I) % n = 0I)
         |> Seq.head

let answer = [1I..999I] |> Seq.maxBy cycleLength // 983.
// Real: 00:00:00.364, CPU: 00:00:00.358, GC gen0: 26, gen1: 1, gen2: 0


// From a comment on that page:
let p26 =
    let get­Se­qLen i =
        let rec loop rem remain­ders rlen =
            match rem with
            | x when List.exists (fun y -> y = x) remain­ders -> rlen
            | x -> let r = ((rem*10)%i)
                   match r with
                   | 0 -> rlen
                   | r -> loop r (x::remainders) (rlen+1)
        loop 1 [] 0

    let find­Max =
        let rec loop maxLen maxI i =
            match i with
            | i when i (maxLen, maxI)
            | 0 -> (maxLen, maxI)
            | i -> let iLen = get­Se­qLen i
                   match iLen > maxLen with
                   | true -> loop iLen i (i-1)
                   | false -> loop maxLen maxI (i-1)
        loop 0 0 1000

    findMax
// Indention was gone. This is my best reconstruction. See the errors, though.


// From http://fsharp-euler.wikispaces.com/euler+026:
let problem26 =
    let recurLen a b = // Problem26.fs(68,5): error FS0588: Block following this 'let' is unfinished. Expect an expression.
        let rec f a b curmap acc =
            if a%b=0 then
                0
            else
                match Map.tryFind a curmap with
                | Some(pos) ->
                    acc - (Map.find a curmap)
                | _ ->
                    f (a%b*10) b (curmap.Add(a, acc)) (acc+1)
        f a b Map.empty 0
 
    {2..999} |> Seq.map (recurLen 1) |> Seq.zip <| {2..999} |> Seq.max
// Well, Hmph. People should test their code before posting it! :)