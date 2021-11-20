namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day04.Part1.answer.Value
        printfn "%A" Day04.Part2.answer.Value
        0