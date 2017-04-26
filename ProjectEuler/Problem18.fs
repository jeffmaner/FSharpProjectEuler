module Problem18

// Find the maximum sum travelling from the top of the triangle to the base.
//
// By starting at the top of the triangle below and moving to adjacent numbers
// on the row below, the maximum total from top to bottom is 23.
// 
// 3
// 7 4
// 2 4 6
// 8 5 9 3
// 
// That is, 3 + 7 + 4 + 9 = 23.
// 
// Find the maximum total from top to bottom of the triangle below:
// 
// 75
// 95 64
// 17 47 82
// 18 35 87 10
// 20 04 82 47 65
// 19 01 23 75 03 34
// 88 02 77 73 07 63 67
// 99 65 04 28 06 16 70 92
// 41 41 26 56 83 40 80 70 33
// 41 48 72 33 47 32 37 16 94 29
// 53 71 44 65 25 43 91 52 97 51 14
// 70 11 33 28 77 73 17 78 39 68 17 57
// 91 71 52 38 17 14 91 43 58 50 27 29 48
// 63 66 04 68 89 53 67 30 73 16 69 87 40 31
// 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
// 
// NOTE: As there are only 16384 routes, it is possible to solve this problem by
// trying every route. However, Problem 67, is the same challenge with a triangle
// containing one-hundred rows; it cannot be solved by brute force, and requires
// a clever method! ;o)


// I like Haskell's solution best, though it's the least comprehensible...
// problem_18 = head $ foldr1 g tri 
//   where
//     f x y z = x + max y z
//     g xs ys = zipWith3 f xs ys $ tail ys
//     tri = [
//         [75],
//         [95,64],
//         [17,47,82],
//         [18,35,87,10],
//         [20,04,82,47,65],
//         [19,01,23,75,03,34],
//         [88,02,77,73,07,63,67],
//         [99,65,04,28,06,16,70,92],
//         [41,41,26,56,83,40,80,70,33],
//         [41,48,72,33,47,32,37,16,94,29],
//         [53,71,44,65,25,43,91,52,97,51,14],
//         [70,11,33,28,77,73,17,78,39,68,17,57],
//         [91,71,52,38,17,14,91,43,58,50,27,29,48],
//         [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
//         [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

let tri = [[75];
           [95;64];
           [17;47;82];
           [18;35;87;10];
           [20;04;82;47;65];
           [19;01;23;75;03;34];
           [88;02;77;73;07;63;67];
           [99;65;04;28;06;16;70;92];
           [41;41;26;56;83;40;80;70;33];
           [41;48;72;33;47;32;37;16;94;29];
           [53;71;44;65;25;43;91;52;97;51;14];
           [70;11;33;28;77;73;17;78;39;68;17;57];
           [91;71;52;38;17;14;91;43;58;50;27;29;48];
           [63;66;04;68;89;53;67;30;73;16;69;87;40;31];
           [04;62;98;27;23;09;70;98;73;93;38;53;60;04;23]]

let f (x, y, z) = x + max y z
let g xs ys = List.map f (List.zip3 xs ys (List.tail ys))
List.head (List.fold g 1 tri) |> printfn "%d"


// Having read the great discussion on of the problem on
// http://theburningmonk.com/2010/09/project-euler-problem-18-solution/, I like
// http://fsharp-euler.wikispaces.com/euler+018:'s solution.

let tri = [|[|75|];
           [|95;64|];
           [|17;47;82|];
           [|18;35;87;10|];
           [|20;04;82;47;65|];
           [|19;01;23;75;03;34|];
           [|88;02;77;73;07;63;67|];
           [|99;65;04;28;06;16;70;92|];
           [|41;41;26;56;83;40;80;70;33|];
           [|41;48;72;33;47;32;37;16;94;29|];
           [|53;71;44;65;25;43;91;52;97;51;14|];
           [|70;11;33;28;77;73;17;78;39;68;17;57|];
           [|91;71;52;38;17;14;91;43;58;50;27;29;48|];
           [|63;66;04;68;89;53;67;30;73;16;69;87;40;31|];
           [|04;62;98;27;23;09;70;98;73;93;38;53;60;04;23|]|]

let problem18 = 
    // Dynamic programming.
    let n = Array.length tri
    for i=1 to n-1 do
        tri.[i].[0] <- tri.[i].[0] + tri.[i-1].[0]
        tri.[i].[i] <- tri.[i].[i] + tri.[i-1].[i-1]
        for j=1 to i-1 do
            tri.[i].[j] <- tri.[i].[j] + max tri.[i-1].[j-1] tri.[i-1].[j]

    tri.[n-1] |> Array.max // 1074.


// The code from the burning monk is just really more complicated, but a little
// easier to follow.

//let triangle = List.ofArray tri
// Use the first definition of tri.
let triangle = tri

// Function to return all the combinations of n elements from the supplied list.
let rec comb n list =
    match n, list with
    | 0, _       -> [[]]
    | _, []      -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ (comb k xs)

// Calculates the next row in the T triangle given the new row in R and the last row in T.
let getNewTotal (row:int list) (total:int list) =
    let head = total.Head
    let tail = List.nth total (total.Length-1)
    let body = total |> Seq.windowed 2 |> Seq.map (fun l -> Seq.max l) |> Seq.toList
    List.map2 (+) row (List.concat [[head]; body; [tail]])

// Recursively traverse down the R triangle and return the last row in T.
let rec traverse (raw:int list list) (total:int list) n =
    let row = raw.[n]
    let newTotal = getNewTotal row total

    if n < (raw.Length-1) then
       traverse raw newTotal (n+1)
    else
       newTotal

let answer = List.max (traverse triangle [75] 1) // 1074.


// From a comment on http://blog.functionalfun.net/2008/08/project-euler-problems-18-and-67.html:
let answer =
    System.IO.File.ReadLines(".\\triangle.txt")
    |> Seq.map(fun x -> x.Split(' ') |> Seq.map(fun y -> System.Int32.Parse(y)))
    |> Seq.fold(fun acc x -> let rowLen = Seq.length x
                             x |> Seq.mapi(fun i y -> let current  () = y+(Seq.nth i acc)
                                                      let previous () = y+(Seq.nth (i-1) acc)
                                                      if i=0 then current ()
                                                      elif i=rowLen-1 then previous ()
                                                      else System.Math.Max(current (), previous ()))
                               |> Seq.toList) [0;]
    |> List.max
