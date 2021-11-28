namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day11.Part1.answer.Value
        printfn "%A" Day11.Part2.answer.Value
        0
