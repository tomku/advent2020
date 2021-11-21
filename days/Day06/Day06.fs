namespace Advent

open FParsec

module Day06 =
    let personP = many1 letter |>> List.distinct .>> newline
    let groupP = many1 personP 
    let formP = sepBy1 groupP newline

    let ParseForms str = 
        match run formP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let forms = Data.ReadFile("day06") |> ParseForms

    module Part1 = 
        let answer = 
            lazy (
                forms 
                |> List.map (List.concat >> List.distinct >> List.length)
                |> List.sum
            )

    module Part2 =
        let answer = 
            lazy (
                forms
                |> List.map (fun group ->
                    let candidates = (List.concat group |> List.distinct)
                    let shared = List.filter (fun c -> List.forall (List.contains c) group) candidates
                    List.length shared
                ) |> List.sum
            )