module Problem11

// What is the greatest product of four adjacent numbers on the same straight
// line in the 20 by 20 grid?
//
// In the 20×20 grid below, four numbers along a diagonal line have been marked
// in brackets.
//
// 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
// 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
// 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
// 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
// 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
// 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
// 32 98 81 28 64 23 67 10 [26] 38 40 67 59 54 70 66 18 38 64 70
// 67 26 20 68 02 62 12 20 95 [63] 94 39 63 08 40 91 66 49 94 21
// 24 55 58 05 66 73 99 26 97 17 [78] 78 96 83 14 88 34 89 63 72
// 21 36 23 09 75 00 76 44 20 45 35 [14] 00 61 33 97 34 31 33 95
// 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
// 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
// 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
// 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
// 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
// 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
// 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
// 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
// 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
// 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
//
// The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
//
// What is the greatest product of four adjacent numbers in any direction (up,
// down, left, right, or diagonally) in the 20×20 grid?

// http://theburningmonk.com/2010/09/project-euler-problem-11-solution/ gives
// the most comprehensible (to me) solution, but it's a lot of code.

open System.IO

let initArray =
    File.ReadAllLines(@"C:\Users\TGHG\Documents\Visual Studio 2010\Projects\ProjectEuler\ProjectEuler\Problem11.txt")
    |> Seq.map (fun l -> l.Split(' ') |> Seq.map int32 |> Seq.toArray)
    |> Seq.toArray

let height, width = initArray.Length, initArray |> Seq.map (fun l -> l.Length) |> Seq.max
let twoDArray = Array2D.init height width (fun i j -> initArray.[i].[j])

let Up (array2D:int[,]) h w n =
    let lowerBound = h-(n-1)
    let upperBound = h
    if lowerBound < 0 || upperBound > height-1 then []
    else [lowerBound..upperBound] |> List.map (fun y -> array2D.[y, w])

let Left (array2D:int[,]) h w n =
    let lowerBound = w-(n-1)
    let upperBound = w
    if lowerBound < 0 || upperBound > width-1 then []
    else [lowerBound..upperBound] |> List.map (fun x -> array2D.[h, x])

let LeftDiag (array2D:int[,]) h w n =
    let lowerWBound = w-(n-1)
    let upperWBound = w
    let lowerHBound = h-(n-1)
    let upperHBound = h
    if lowerWBound < 0 || upperWBound > width-1 || lowerHBound < 0 || upperHBound > height-1
    then []
    else
        let wCoordinates = [lowerWBound..upperWBound]
        let hCoordinates = [lowerHBound..upperHBound]
        List.map2 (fun y x -> array2D.[y, x]) hCoordinates wCoordinates

let RightDiag (array2D:int[,]) h w n =
    let lowerWBound = w
    let upperWBound = w+(n-1)
    let lowerHBound = h-(n-1)
    let upperHBound = h
    if lowerWBound < 0 || upperWBound > width-1 || lowerHBound < 0 || upperHBound > height-1
    then []
    else
        let wCoordinates = [lowerWBound..upperWBound]
        let hCoordinates = [lowerHBound..upperHBound] |> List.rev
        List.map2 (fun y x -> array2D.[y, x]) hCoordinates wCoordinates

let quartets =
    seq { for y in 3 .. width-1 do
              for x in 3 .. height-1 do
                  yield Up twoDArray x y 4
                  yield Left twoDArray x y 4
                  yield LeftDiag twoDArray x y 4
                  yield RightDiag twoDArray x y 4 }

let CalcProduct numbers = numbers |> Seq.fold (fun acc n -> acc * n) 1
let maxProduct = quartets |> Seq.map CalcProduct |> Seq.max // 70600674.


// From http://fsharp-euler.wikispaces.com/euler+011, much less code, but I'm
// not sure I know what's going on.

let readMatrix filename =
    let lines = File.ReadAllLines(filename)
    lines |> Array.map (fun line -> (line.Split([|' '|]) |> Array.map int))

let problem11 =
    let m = readMatrix @"C:\Users\TGHG\Documents\Visual Studio 2010\Projects\ProjectEuler\ProjectEuler\problem11.txt"
    let nrow = Array.length m
    let ncol = Array.length m.[0]
    let collect i j di dj =
        let rec rcollect i j step acc =
            if step=0 then
                acc
            elif i<0 || i>=nrow || j<0 || j>=ncol then
                0
            else
                rcollect (i+di) (j+dj) (step-1) acc*m.[i].[j]
        rcollect i j 4 1

    let goRight i j = collect i j 0  1
    let goDown  i j = collect i j 1  0
    let goDiag1 i j = collect i j 1  1
    let goDiag2 i j = collect i j 1 -1
    seq {for i=1 to nrow do
             for j=1 to ncol do
                 yield [goRight i j; goDown i j; goDiag1 i j; goDiag2 i j] |> List.max}
    |> Seq.max // 70600674, and much faster...