namespace Advent

open FParsec

module Data =
    let ReadFile (name: string) =
        use r =
            new System.IO.StreamReader("data/" + name + ".txt")

        r.ReadToEnd()