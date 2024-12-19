#load "Common.fsx"

open System

let sample =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

type ReportType =
    | Unsafe = 0
    | Safe = 1

let isSafe x y diffSign =
    let diff = abs (x-y)
    diff >= 1 && diff <= 3 && sign (x-y) = diffSign

let checkReport (levels: int array)=
    let rec checkLevel i =
        if levels.Length < 2 then ReportType.Unsafe
        else if i >= levels.Length then ReportType.Safe
        else
            match isSafe levels[i] levels[i-1] (sign (levels[1] - levels[0])) with
            | true -> checkLevel (i+1)
            | false -> ReportType.Unsafe
                      
    checkLevel 1

let parseReport line =
    match line with
    | Split ' ' levels -> levels |> List.map int |> Array.ofList

let parse lines =
    lines
    |> Array.map (parseReport >> checkReport >> int)
    |> Array.sum

sample.SplitOnNewLine() |> parse
Files[2] |> parse
// answer: 246

// Part 2

let removeAt i (arr: int array) =
    Array.append arr.[0..i-1] arr.[i+1..]

let checkReport' (levels: int array)=
    let rec loop i (arr: int array)  =
        if i > levels.Length then ReportType.Unsafe
        else
            match checkReport arr with
            | ReportType.Safe -> ReportType.Safe
            | _ -> loop (i+1) (removeAt i levels)
    
    loop 0 levels

let parse' lines =
    lines
    |> Array.map (parseReport >> checkReport' >> int)
    |> Array.sum
    
    
sample.SplitOnNewLine() |> parse'
Files[2] |> parse'
// answer: 318
