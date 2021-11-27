namespace Advent

open FParsec

module Day09 =
    let numbersP = sepEndBy1 (pint64 |>> int) newline

    let numbers =
        Data.readFile "day09" |> Data.parse numbersP

    let window = 25

    let validationSets =
        List.windowed (window + 1)
        >> List.map (fun xs -> (xs |> List.take window, List.last xs))

    let valid (xs, n) =
        List.exists
            (fun x ->
                xs
                |> List.filter (fun y -> y <> x)
                |> List.contains (n - x))
            xs

    module Part1 =
        let answer =
            lazy
                (numbers
                 |> validationSets
                 |> List.find (not << valid))
                |> snd

    module Part2 =
        let tails xs =
            Seq.init (List.length xs) (fun n -> List.skip n xs)

        let partialSumsUpTo n =
            Seq.scan (fun x y -> x + y) 0
            >> Seq.skip 1
            >> Seq.takeWhile (fun z -> z <= n)

        let answer =
            lazy
                (let target = Part1.answer.Value

                 numbers
                 |> tails
                 |> Seq.map
                     (fun t ->
                         t
                         |> Seq.zip (partialSumsUpTo target t)
                         |> Seq.map snd)
                 |> Seq.find (fun x -> Seq.sum x = target)
                 |> (fun xs -> Seq.min xs + Seq.max xs))
