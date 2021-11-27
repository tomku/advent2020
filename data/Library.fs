﻿namespace Advent

open System.Resources
open FParsec

module Data =
    let readFile (name: string) =
        let res =
            ResourceManager("Data.Data", System.Reflection.Assembly.GetExecutingAssembly())

        res.GetString(name)

    let parse parser str =
        match run parser str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error
