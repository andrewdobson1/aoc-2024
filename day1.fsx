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
    |> (fun xs -> [for x in xs -> fst x], [for x in xs -> snd x])
    |> (fun (x,y) -> List.sort x, List.sort y)
    |> (fun (x,y) -> List.zip x y)
    |> List.map (fun (x,y) -> Math.Abs(x - y))
    |> List.sum

//sample.SplitOnNewLine() |> parse
//Files[1] |> parse

// Part 2

let parse' lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [left; right] -> Some ((int left), (int right))
        | _ -> None)
    |> Array.choose id
    |> (fun xs -> [for x in xs -> fst x], [for x in xs -> snd x])
    |> (fun (xs,ys) ->
        xs
        |> List.map (fun x -> x * (ys |> List.filter (fun y -> y = x) |> List.length))
    )
    |> List.sum
    
//sample.SplitOnNewLine() |> parse'
Files[1] |> parse'    

