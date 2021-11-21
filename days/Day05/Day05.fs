namespace Advent

open FParsec

module Day05 =
    type SeatPos = { Row: int; Col: int }
    let seatID pos = pos.Row * 8 + pos.Col

    let interpretDirs rows cols =
        {
            Row = List.ofArray rows |> List.mapi (fun i x -> x * (pown 2 (6-i) )) |> List.sum;
            Col = List.ofArray cols |> List.mapi (fun i x -> x * (pown 2 (2-i) )) |> List.sum
        }

    let frontBackP = (pchar 'F' >>% 0) <|> (pchar 'B' >>% 1)
    let leftRightP = (pchar 'L' >>% 0) <|> (pchar 'R' >>% 1)
    let boardingPassP = pipe2 (parray 7 frontBackP) (parray 3 leftRightP) interpretDirs .>> newline
    let boardingPassListP = many1 boardingPassP

    let ParseBoardingPasses str = 
        match run boardingPassListP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let boardingPasses = Data.ReadFile("day05") |> ParseBoardingPasses    

    module Part1 =
        let answer = lazy ( boardingPasses |> List.map seatID |> List.max )

    module Part2 =
        let answer = 
            lazy ( 
                boardingPasses 
                |> List.sortBy seatID 
                |> List.pairwise 
                |> List.find (fun (x, y) -> seatID y - seatID x > 1) 
                |> (fun (x, _) -> seatID(x) + 1)
            )
