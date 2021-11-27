namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day07.Part1.answer.Value
        printfn "%A" Day07.Part2.answer.Value
        0
