module Problem24

// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
//
// A permutation is an ordered arrangement of objects. For example, 3124 is one
// possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
// are listed numerically or alphabetically, we call it lexicographic order. The
// lexicographic permutations of 0, 1 and 2 are:
//
// 012   021   102   120   201   210
//
// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

// From http://theburningmonk.com/2010/09/project-euler-problem-24-solution/,
// which cited /F# for Scientists/ for the first two functions, so they are *way*
// beyond me...

let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let getLexicographic (list:bigint list) =
    list
    |> permute
    |> List.map (fun l -> l |> List.rev |> List.mapi (fun i e -> e * pown 10I i) |> List.sum)
    |> List.sort

let answer = List.nth (getLexicographic [0I..9I]) (1000000-1) // 2783915460.
// Real: 00:00:58.333, CPU: 00:00:59.155, GC gen0: 2878, gen1: 312, gen2: 7

// Haskell solution from http://www.haskell.org/haskellwiki/Euler_problems/21_to_30:
// import Data.List 
// 
// fac 0 = 1
// fac n = n * fac (n - 1)
// perms [] _= []
// perms xs n= x : perms (delete x xs) (mod n m)
//   where m = fac $ length xs - 1
//         y = div n m
//         x = xs!!y
// 
// problem_24 = perms "0123456789" 999999

let rec fac = function
    | 0 -> 1
    | n -> n * fac (n-1)

let rec delete x xs =
    match xs with
    | [] -> []
    | y::ys -> if x=y then ys else y :: delete x ys

let rec perms s n =
    match s with
    | [] -> []
    | c::cs -> let m = s |> List.length |> (fun z -> z-1) |> fac
               let y = n / m
               let x = cs.[y]
               x :: perms (delete x cs) (n%m)

let problem24 = perms ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"] 999999
// Hmm. I let it run for several minutes, and got no solution. Not sure if my implementation is bad,
// or if it's really just a slow algorithm.