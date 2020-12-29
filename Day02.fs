namespace Advent

open System
open FSharp.Collections
open FParsec

module Day02 =
    let passwords = Data.ReadFile("day02")
    type Policy = {Min:int; Max:int; Letter:char}
    
    let minP = pint32 .>> pstring "-"
    let maxP = pint32 .>> spaces
    let reqP = anyChar .>> pstring ":" .>> spaces
    let passwordP = restOfLine true
    let policyP = pipe3 minP maxP reqP (fun min max req -> {Min= min; Max= max; Letter= req})
    let passwordPolicyLineP = pipe2 policyP passwordP 

    let auditWith auditor str = 
        match run (many (passwordPolicyLineP auditor)) str with
            | Success (result, _, _) -> result
            | Failure (error, _, _) -> failwith error

    let countTrue = List.filter (id) >> List.length

    module Part1 = 
        let auditMethod (policy: Policy) (pw: string) = 
            let counts = pw |> Seq.toList |> List.countBy id |> Map.ofList
            match counts |> (Map.tryFind policy.Letter) with
                | Some x -> x >= policy.Min && x <= policy.Max
                | None -> policy.Min = 0
        
        let answer = lazy (
            passwords |> auditWith auditMethod |> countTrue
        )

    module Part2 =
        let xor a b = (a || b) && not (a && b)

        let newAuditMethod (policy: Policy) (pw: string) = 
            policy.Min <= pw.Length && policy.Max <= pw.Length && policy.Min >= 1 && policy.Max >= 1 &&
            xor (pw.[policy.Min-1].Equals(policy.Letter)) (pw.[policy.Max-1].Equals(policy.Letter))

        let answer = lazy (
            passwords |> auditWith newAuditMethod |> countTrue
        ) 

