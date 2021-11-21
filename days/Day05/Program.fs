namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day05.Part1.answer.Value
        printfn "%A" Day05.Part2.answer.Value
        0