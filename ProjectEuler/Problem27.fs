module Problem27

// Find a quadratic formula that produces the maximum number of primes for consecutive values of n.
//
// Euler published the remarkable quadratic formula:
//
// n² + n + 41
//
// It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However,
// when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41,
// 41² + 41 + 41 is clearly divisible by 41.
//
// Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for
// the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
//
// Considering quadratics of the form:
//
//    n² + an + b, where |a| < 1000 and |b| < 1000
//
//    where |n| is the modulus/absolute value of n
//    e.g. |11| = 11 and |−4| = 4
//
// Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum
// number of primes for consecutive values of n, starting with n = 0.


// Haskell solution:
//problem_27 = -(2*a-1)*(a^2-a+41)
//  where n = 1000
//        m = head $ filter (\x->x^2-x+41>n) [1..]
//        a = m-1
let naturalNumbers = Seq.unfold (fun state -> Some(state, state+1)) 1
let problem_27 =
    let n = 1000
    let m = naturalNumbers |> Seq.filter (fun x -> (pown x 2)-x+41>n) |> Seq.head
    let a = m-1
    -(2*a-1)*((pown a 2)-a+41) // -59231.
// I've no idea what's going on here, but it's very little code and pretty fast.
// Real: 00:00:00.001, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0


// From http://theburningmonk.com/2010/09/project-euler-problem-27-solution/:

let hasDivisor n =
    let m = int64 (sqrt (double n))
    [2L..m] |> Seq.exists (fun x -> x%n=0L)

let isPrime n = if n <= 1L then false else not (hasDivisor n)

// The quadratic expression.
let F n a b = n*n + a*n + b

// Returns the number of consecutive primes the coefficients generate.
let primeCount a b =
    Seq.unfold (fun state -> Some(state, state + 1L)) 0L
    |> Seq.takeWhile (fun n -> isPrime (F n a b))
    |> Seq.length

let aList, bList = [-999L..999L], [2L..999L] |> List.filter isPrime
// Real: 00:00:00.049, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

let answer =
    let (a, b, _) =
        aList
        |> List.collect (fun a -> bList
                                  |> List.filter (fun b -> a+b >= 1L)
                                  |> List.map (fun b -> (a, b, primeCount a b)))
        |> List.maxBy (fun (_, _, count) -> count)
    a*b
// Ran for several minutes without returning an answer. Killed it.


// From http://fsharp-euler.wikispaces.com/euler+027:


let problem27 =
    let isPrime n =
        if n=1 then false
        else let m = int (sqrt (float n)) in
             {2..m} |> Seq.forall (fun i -> n%i<>0)

    let notPrime x = x <= 0 || not (isPrime x)
    let count a b =
        {0..System.Int32.MaxValue} |> Seq.map (fun n -> n*n + a*n + b) |> Seq.findIndex notPrime

    seq {for i = -999 to 999 do
             for j = -999 to 999 do
                 yield ((count i j), i, j)} |> Seq.max // (71, -61, 971).
// Real: 00:00:07.740, CPU: 00:00:07.690, GC gen0: 634, gen1: 1, gen2: 0