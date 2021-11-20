namespace Advent

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "%A" Day03.Part1.answer.Value
        printfn "%u" Day03.Part2.answer.Value
        0 // return an integer exit code