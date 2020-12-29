namespace Advent

open FParsec

module Day03 =
    type Cell = 
    | Tree
    | Open
    | Finish

    type Position = {Terrain: Cell list list; X: int; Y: int}

    type Vector = {DX: int; DY: int}

    let makeCell = function
    | "#" -> Tree
    | "." -> Open
    | x -> failwithf "Unknown map tile: %A" x

    let makePosition cells = {Terrain=cells; X=0; Y=0}
    
    let cellP = pstring "#" <|> pstring "." |>> makeCell
    let rowP = many1 cellP .>> newline
    let terrainP = many1 rowP

    let ParseTerrain str =
        match run terrainP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let pos = Data.ReadFile("day03") |> ParseTerrain |> makePosition

    let standingOn = function
     | {Terrain=t; X=x; Y=y} ->
         if y >= t.Length then 
             Finish 
         else
             let row = t.[y]
             row.[x % row.Length]        

    let move vec terrain = 
        let newPos = {terrain with X=terrain.X+vec.DX; Y=terrain.Y+vec.DY}
        Some (standingOn newPos, newPos)
 
    let path (pos: Position) (vec: Vector) =
        Seq.unfold (move vec) pos |> Seq.takeWhile (fun x -> x <> Finish) 

    let treesOnPath pos vec =
        path pos vec |> Seq.filter (fun x -> x = Tree) |> Seq.length

    module Part1 = 
        let answer = lazy (treesOnPath pos {DX=3; DY=1})

    module Part2 =
        let vecs = [{DX=1; DY=1}; {DX=3; DY=1}; {DX=5; DY=1}; {DX=7; DY=1}; {DX=1; DY=2}]
        let answer = lazy (vecs |> List.map (treesOnPath pos) |> List.reduce (*))
