namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day10.Part1.answer.Value
        printfn "%A" Day10.Part2.answer.Value
        0
