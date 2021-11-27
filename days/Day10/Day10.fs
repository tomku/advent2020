namespace Advent

open FParsec

module Day10 =

    let numbersP = sepEndBy1 (pint64 |>> int) newline

    let numbers =
        "day10" |> Data.readFile |> Data.parse numbersP

    module Part1 =
        let answer =
            lazy
                (numbers
                 |> List.append [ List.max numbers + 3; 0 ]
                 |> List.sort
                 |> List.pairwise
                 |> List.map (fun (y, x) -> x - y)
                 |> List.countBy id
                 |> Map.ofList
                 |> fun m -> Map.find 1 m * Map.find 3 m)

    module Part2 =
        let desc =
            numbers
            |> List.append [ List.max numbers + 3; 0 ]
            |> List.sort
            |> List.rev

        let possiblePathCounts = Map.ofList [ (List.head desc, 1I) ]

        let extendPathUsing map n =
            let pathsStartingHere =
                [ 1; 2; 3 ]
                |> List.map (fun x -> n + x)
                |> List.choose (fun x -> Map.tryFind x map)
                |> List.sum

            Map.add n pathsStartingHere map

        let answer =
            lazy
                (desc
                 |> List.tail
                 |> List.fold extendPathUsing possiblePathCounts
                 |> Map.find 0)
