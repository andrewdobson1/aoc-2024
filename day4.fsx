#load "Common.fsx"

open System

let sample =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""
    
let directions = 
    [| (0, -1); (0, 1); (1, 0); (-1, 0); (1, -1); (1, 1); (-1, -1); (-1, 1) |]
    
let check x y (word:string) (dx, dy) (arr: char [][]) =
    let chars = word |> Seq.toArray
    let rec loop x y i =
        if i >= chars.Length then true
        else if x < 0 || y < 0 || y >= arr.Length || x >= arr[y].Length then false
        else if chars[i] <> arr[y][x] then false
        else loop (x + dx) (y + dy) (i+1)
    
    loop x y 0
    
let countWordsAt word x y arr=
    directions
    |> Array.sumBy (fun (dx, dy) -> if check x y word (dx, dy) arr then 1 else 0)

let countWords word arr =
    arr
    |> Array.mapi (fun y xs ->
        xs
        |> Array.mapi (fun x _ -> countWordsAt word x y arr)
        |> Array.sum)
    
    |> Array.sum
    
let parse lines =
    lines
    |> Array.map Seq.toArray
    |> Array.filter (fun xs -> xs.Length > 0)
    |> countWords "XMAS"

sample.SplitOnNewLine() |> parse
Files[4] |> parse
// answer: 2370

// Part 2

let isMAS v =
    match v with
    | 'M','S' | 'S','M' -> true
    | _ -> false

let isXMAS x y (arr: char [][]) =
    if x-1 < 0 || y-1 < 0 || y+1 >= arr.Length || x+1 >= arr[y].Length then false
    else if arr[y][x] <> 'A' then false
    else
        (isMAS (arr[y-1][x-1],arr[y+1][x+1])) && (isMAS (arr[y-1][x+1],arr[y+1][x-1]))
        
let countXMAS arr =
    arr
    |> Array.mapi (fun y xs ->
        xs
        |> Array.mapi (fun x _ -> if (isXMAS x y arr) then 1 else 0)
        |> Array.sum)
    
    |> Array.sum  

let parse' lines =
    lines
    |> Array.map Seq.toArray
    |> Array.filter (fun xs -> xs.Length > 0)
    |> countXMAS

sample.SplitOnNewLine() |> parse'
Files[4] |> parse'
// answer: 1908