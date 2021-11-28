namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day12.Part1.answer.Value
        printfn "%A" Day12.Part2.answer.Value
        0
