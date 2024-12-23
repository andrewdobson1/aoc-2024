#load "Common.fsx"

open System
open System.Security
open System.Text.RegularExpressions

let sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let parseLine s =
    Regex.Matches(s, "mul\((\d{1,3}),(\d{1,3})\)")
    |> Seq.map (fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value))
    |> Array.ofSeq

let parse lines =
    lines
    |> Array.collect parseLine
    |> Array.sum

sample.SplitOnNewLine() |> parse
Files[3] |> parse
// answer: 173529487

// Part 2

let sample2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let parseLine' s =
    Regex.Matches(s, "(?<mul>mul)\((\d{1,3}),(\d{1,3})\)|(?<dont>don't)\(\)|(?<do>do)\(\)")
    |> Seq.fold (fun s m ->
        let total, continueFlag = s
        match m with
        | _ when m.Groups["dont"].Success -> fst s, false
        | _ when m.Groups["do"].Success -> fst s, true
        | _ when m.Groups["mul"].Success && continueFlag ->
            total + (int m.Groups[1].Value) * (int m.Groups[2].Value), true
        | _ -> s
        ) (0, true)
    |> fst

let parse' (lines:string array) =
    lines
    |> (fun xs -> String.Join("", xs))
    |> parseLine'    

sample2.SplitOnNewLine() |> parse'
Files[3] |> parse'
// answer: 99532691