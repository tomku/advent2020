namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day06.Part1.answer.Value
        printfn "%A" Day06.Part2.answer.Value
        0