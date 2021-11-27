namespace Advent

open FParsec

module Day08 =
    type Instr = 
        | Nop of int
        | Incr of int
        | Jump of int

    let nopP = pstring "nop " >>. pint64 |>> (int >> Nop)
    let incrP = pstring "acc " >>. pint64 |>> (int >> Incr)
    let jumpP = pstring "jmp " >>. pint64 |>> (int >> Jump)

    let instrP = nopP <|> incrP <|> jumpP
    let programP = sepEndBy1 instrP newline

    let ParseProgram str = 
        match run programP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    type State = { Line: int; Acc: int; Program: List<Instr>; Executed: Set<int> }
    let initState prg = { Line=0; Acc=0; Program=prg; Executed=Set.empty }

    let interpret state =
        match (List.tryItem state.Line state.Program, Set.contains state.Line state.Executed) with
            | Some (Nop _), false -> 
                let newState = {state with Line=state.Line + 1; Executed=Set.add state.Line state.Executed}
                Some (newState, newState)
            | Some (Incr n), false ->
                let newState = {state with Line=state.Line + 1; Executed=Set.add state.Line state.Executed; Acc=state.Acc + n}
                Some (newState, newState)
            | Some (Jump d), false -> 
                let newState = {state with Line=state.Line + d; Executed=Set.add state.Line state.Executed}
                Some (newState, newState)
            | _ -> None

    let execute prg = Seq.unfold interpret (initState prg)

    let program = Data.ReadFile "day08" |> ParseProgram

    module Part1 =
        let answer = lazy ( program |> execute |> Seq.last |> (fun s -> s.Acc) )
        
    module Part2 =
        let toggleNopJump = function
            | Nop x -> Jump x
            | Jump x -> Nop x
            | Incr n -> Incr n

        let mutate = 
            List.indexed 
            >> List.filter (function (_, Incr _) -> false | _ -> true)
            >> List.map (fun (i, instr) -> List.updateAt i (toggleNopJump instr) program)

        let terminates states =
            Seq.tryLast states |> Option.map (fun last -> last.Line = last.Program.Length) |> Option.contains true

        let answer = lazy ( program |> mutate |> Seq.map execute |> Seq.find terminates |> Seq.last)