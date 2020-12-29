namespace Advent

open System
open FSharp.Collections

module Day01 =
    let target = 2020L

    let numbers =
        lazy (Data.ReadFile("day01") |> Data.Numbers)

    module Part1 =
        let findPairSummingTo sum xs =
            let db = Set.empty

            let update (db, found) n =
                match found with
                | Some _ -> (db, found)
                | None -> if db |> Set.contains (sum - n) then (db |> Set.add n, Some n) else (db |> Set.add n, None)

            match List.fold update (db, None) xs with
            | (_, Some n) -> Some(n, sum - n)
            | (_, None) -> None

        let answer =
            lazy
                (match findPairSummingTo target numbers.Value with
                 | Some (n1, n2) -> n1 * n2
                 | None -> failwith "Failed to find answer to problem #1")

    module Part2 = 
        let rec findTripleSummingTo sum =
            function
            | x :: rest ->
                match Part1.findPairSummingTo (target - x) rest with
                | Some (n1, n2) -> Some(x, n1, n2)
                | None -> findTripleSummingTo sum rest
            | _ -> None

        let answer =
            lazy
                (match findTripleSummingTo target numbers.Value with
                 | Some (n1, n2, n3) -> n1 * n2 * n3
                 | None -> failwith "Failed to find answer to problem #2")
