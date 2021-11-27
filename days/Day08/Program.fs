namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day08.Part1.answer.Value
        printfn "%A" Day08.Part2.answer.Value
        0