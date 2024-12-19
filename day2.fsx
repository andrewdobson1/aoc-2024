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

type Direction =
    | None = 0
    | Increasing = 1
    | Descrasing = 2

type ReportType =
    | Unsafe = 0
    | Safe = 1

let checkReport (levels: int array)=
    let rec checkLevel i direction =
        if levels.Length < 2 then ReportType.Unsafe
        else if i >= levels.Length then ReportType.Safe
        else
            match levels[i] - levels[i-1] with
            | 0 -> ReportType.Unsafe
            | x when x <= -1 && x >= -3 ->
                match direction with
                | Direction.None | Direction.Descrasing -> checkLevel (i+1) Direction.Descrasing
                | _ -> ReportType.Unsafe
            | x when x >= 1 && x <= 3 ->
                match direction with
                | Direction.None | Direction.Increasing -> checkLevel (i+1) Direction.Increasing
                | _ -> ReportType.Unsafe
            | _ -> ReportType.Unsafe
                      
    checkLevel 1 Direction.None

let parseReport line =
    match line with
    | Split ' ' levels -> levels |> List.map int |> Array.ofList

let parse lines =
    lines
    |> Array.map (parseReport >> checkReport >> int)
    |> Array.sum

sample.SplitOnNewLine() |> parse
Files[2] |> parse

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
