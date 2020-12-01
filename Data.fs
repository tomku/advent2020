namespace Advent

open FParsec

module Data =
    let ReadFile (name: string) =
        use r =
            new System.IO.StreamReader("data/" + name + ".txt")

        r.ReadToEnd()

    let whitespaceP = many (anyOf "\r\n\t ")

    let numberP = sepEndBy pint64 whitespaceP

    let Numbers str =
        match run numberP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error
