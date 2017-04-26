module Problem19

// How many Sundays fell on the first of the month during the twentieth century?
//
// You are given the following information, but you may prefer to do some research for yourself.
// 
//     * 1 Jan 1900 was a Monday.
//     * Thirty days has September,
//       April, June and November.
//       All the rest have thirty-one,
//       Saving February alone,
//       Which has twenty-eight, rain or shine.
//       And on leap years, twenty-nine.
//     * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
// 
// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


// From http://theburningmonk.com/2010/09/project-euler-problem-19-solution/:

open System

let answer =
    [1901..2000]
    |> List.collect (fun y -> [1..12] |> List.map (fun m -> new DateTime(y, m, 1)))
    |> List.filter (fun d -> d.DayOfWeek = DayOfWeek.Sunday)
    |> List.length // 171.

// Wow! :)


// From http://fsharp-euler.wikispaces.com/euler+019:

let problem19b =
    let beginDay = System.DateTime.Parse("1/1/1901")
    let endDay = System.DateTime.Parse("12/31/2000")
    (* unfold to get a infinite sequence of days beginning beginDay *)
    let days = beginDay |> Seq.unfold (fun s -> Some(s, s.AddDays 1.0))

    days
    |> Seq.takeWhile (fun day -> day <= endDay)
    |> Seq.filter (fun day -> day.Day = 1 && day.DayOfWeek = System.DayOfWeek.Sunday)
    |> Seq.length // 171.