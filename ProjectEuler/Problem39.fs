module Problem39

// If p is the perimeter of a right angle triangle, {a, b, c}, which value, for p ≤ 1000, has the most
// solutions?
//
// If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly
// three solutions for p = 120.
//
// {20,48,52}, {24,45,51}, {30,40,50}
//
// For which value of p ≤ 1000, is the number of solutions maximised?

let isRightTriangle a b c = a*a + b*b = c*c

let perimeter (a,b,c) = a+b+c

let rightTriangles =
    seq {for a in [1..1000] do
             for b in [a..1000] do
                 for c in [b..1000] do
                     if isRightTriangle a b c then
                        yield (a,b,c)}
// Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0

let myAnswer =
    rightTriangles
    |> Seq.groupBy perimeter
    |> Seq.filter (fun (p,ts) -> p<=1000)
    |> Seq.maxBy (fun (p,ts) -> Seq.length ts) 
    |> (fun (p,ts) -> (p, Seq.length ts)) // (840, 8).
// Real: 00:01:05.818, CPU: 00:01:05.738, GC gen0: 3486, gen1: 6, gen2: 0


// Okay, now to the professionals... :)

// Okay, these are completely foreign to me.

// From http://theburningmonk.com/2010/10/project-euler-problem-39-solution/:

let countSolutions p =
    [4*p/10..6*p/10]
    |> List.filter (fun c -> [1..p]
                             |> Seq.takeWhile (fun b -> b + c < p)
                             |> Seq.exists (fun b -> (pown (p-b-c) 2 + pown b 2) = pown c 2))
    |> List.length

let answer = [1..1000] |> List.maxBy countSolutions // 840. I got it right! :)
// Real: 00:00:08.380, CPU: 00:00:08.330, GC gen0: 1385, gen1: 1, gen2: 0


// From http://fsharp-euler.wikispaces.com/euler+039:

let problem39 =
    let count p =
        let n = p/2
        let sqr n = n*n
        seq {for i=2 to n do
                 for j=i+1 to n do
                     if sqr i + sqr j = sqr (p-i-j) then
                        yield (i,j)} |> Seq.length
    {10..1000} |> Seq.map (fun n -> count n, n) |> Seq.max // (8, 840).
// Real: 00:00:15.169, CPU: 00:00:15.100, GC gen0: 34, gen1: 1, gen2: 0


// From http://www.mathblog.dk/pythagorean-triplets/, generating Pythagorean Triplets by brute force:
let pythagoreanTriplets limit =
    seq {for a in [1..(limit/3)] do     // He states that the fact a<b<c yields the facts that
             for b in [a..(limit/2)] do // a<limit/3 and a<b<limit/2. I don't get that derivation.
                 let c = limit - a - b
                 if a*a + b*b = c*c then
                    yield (a,b,c)}

// Math Blog has excellent discussions on better solutions to the problem--faster, that is. One is
// arithmetic, the other is number theoretical.

// I can't figure out how I would implement the arithmetic solution functionally, so I'll just move on
// to VB.