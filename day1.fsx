#load "Common.fsx"

open System

let sample =
    """3   4
4   3
2   5
1   3
3   9
3   3
"""

let parse lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [left; right] -> Some ((int left), (int right))
        | _ -> None)
    |> Array.choose id
    |> Array.unzip
    |> (fun (x,y) -> Array.sort x, Array.sort y)
    |> (fun (x,y) -> Array.zip x y)
    |> Array.map (fun (x,y) -> abs (x - y))
    |> Array.sum

sample.SplitOnNewLine() |> parse
Files[1] |> parse

// Part 2

let parse' lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [left; right] -> Some ((int left), (int right))
        | _ -> None)
    |> Array.choose id
    |> Array.unzip
    |> (fun (xs,ys) ->
        xs
        |> Array.map (fun x -> x * (ys |> Array.filter (fun y -> y = x) |> Array.length))
    )
    |> Array.sum
    
sample.SplitOnNewLine() |> parse'
Files[1] |> parse'    

