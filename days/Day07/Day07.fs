namespace Advent

open FParsec

module Day07 =
    type BagSpec = { Count: int; Color: string }
    let makeBagSpec n color = { Count=n; Color=color }
    
    let colorP = charsTillString " bags contain " true 255
    let bagSpecP = pipe2 (pint64 .>> many1Chars (pchar ' ') |>> int) (charsTillString " bag" true 255) makeBagSpec
    let sepP = optional (pstring "s") >>. (pstring ", " <|> pstring ".")
    let emptyP = pstring "no other bags." >>% None
    let bagSpecListP = sepEndBy1 bagSpecP sepP |>> Some
    let ruleP = tuple2 colorP (emptyP <|> bagSpecListP)
    let rulesP = many1 (ruleP .>> newline) |>> Map.ofList

    let ParseRules str = 
        match run rulesP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let rules = Data.ReadFile("day07") |> ParseRules

    module Part1 =
        let rec canContain env target key = 
            match Map.find key env with
            | None -> false
            | Some specs -> specs |> List.exists (fun spec -> spec.Color = target || canContain env target spec.Color)
            
        let answer = lazy ( rules |> Map.filter (fun k _v -> canContain rules "shiny gold" k) |> Map.count )
        
    module Part2 =
        let rec countContents env = function
        | None -> 0
        | Some specs -> List.fold (fun acc x -> x.Count * (1 + countContents env (Map.find x.Color env)) + acc) 0 specs

        let answer = lazy ( countContents rules rules["shiny gold"] )