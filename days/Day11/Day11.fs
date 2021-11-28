namespace Advent

open FParsec

module Day11 =
    type Seat =
        | Empty
        | Occupied
        | Floor

    let floorP = pchar '.' >>% Floor
    let seatP = pchar 'L' >>% Empty
    let spaceP = floorP <|> seatP
    let rowP = many1 spaceP .>> newline
    let areaP = many1 rowP

    let areaMatrix =
        "day11" |> Data.readFile |> Data.parse areaP

    let dims =
        (areaMatrix |> List.head |> List.length, areaMatrix |> List.length)

    let areaMap =
        areaMatrix
        |> List.map List.indexed
        |> List.indexed
        |> List.collect (fun (y, row) -> List.map (fun (x, s) -> ((x, y), s)) row)
        |> Map.ofList

    let neighborOffsets =
        [ (-1, -1)
          (0, -1)
          (1, -1)
          (-1, 0)
          (1, 0)
          (-1, 1)
          (0, 1)
          (1, 1) ]

    let coords =
        seq {
            for x in 0 .. fst dims - 1 do
                for y in 0 .. snd dims - 1 do
                    (x, y)
        }

    let offset (x, y) (dx, dy) = (x + dx, y + dy)

    let stepper visibilityRule survivalRule oldGen =
        let updateCell newGen xy =
            let neighborsFilled =
                visibilityRule xy oldGen
                |> List.filter (fun x -> x = Occupied)
                |> List.length

            Map.add xy (survivalRule (Map.find xy oldGen) neighborsFilled) newGen

        Seq.fold updateCell oldGen coords

    let run visibilityRule survivalRule =
        Seq.unfold
            (fun area ->
                let newArea = stepper visibilityRule survivalRule area

                if newArea = area then
                    None
                else
                    Some(newArea, newArea))

    module Part1 =
        let immediateNeighborRule xy oldGen =
            neighborOffsets
            |> List.choose (fun n -> Map.tryFind (offset xy n) oldGen)

        let zeroGrowsFourDies curr neighbors =
            match curr with
            | Empty when neighbors = 0 -> Occupied
            | Occupied when neighbors >= 4 -> Empty
            | other -> other

        let answer =
            lazy
                (areaMap
                 |> run immediateNeighborRule zeroGrowsFourDies
                 |> Seq.last
                 |> Map.values
                 |> Seq.countBy id)

    module Part2 =
        let lineOfSightRule xy oldGen =
            neighborOffsets
            |> List.choose
                (fun (x, y) ->
                    Seq.initInfinite (fun n -> Map.tryFind (offset xy (n * x, n * y)) oldGen)
                    |> Seq.skip 1
                    |> Seq.takeWhile (fun o -> o.IsSome)
                    |> Seq.choose id
                    |> Seq.tryFind (fun s -> s <> Floor))

        let zeroGrowsFiveDies curr neighbors =
            match curr with
            | Empty when neighbors = 0 -> Occupied
            | Occupied when neighbors >= 5 -> Empty
            | other -> other

        let answer =
            lazy
                (areaMap
                 |> run lineOfSightRule zeroGrowsFiveDies
                 |> Seq.last
                 |> Map.values
                 |> Seq.countBy id)
