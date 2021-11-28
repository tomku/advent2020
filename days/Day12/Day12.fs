namespace Advent

open FParsec

module Day12 =
    type Command =
        | GoNorth of int
        | GoEast of int
        | GoWest of int
        | GoSouth of int
        | TurnLeft of int
        | TurnRight of int
        | GoForward of int

    let northP =
        pchar 'N' >>. pint64 |>> (int >> GoNorth)

    let southP =
        pchar 'S' >>. pint64 |>> (int >> GoSouth)

    let eastP = pchar 'E' >>. pint64 |>> (int >> GoEast)

    let westP = pchar 'W' >>. pint64 |>> (int >> GoWest)

    let leftP =
        pchar 'L' >>. pint64 |>> (int >> TurnLeft)

    let rightP =
        pchar 'R' >>. pint64 |>> (int >> TurnRight)

    let forwardP =
        pchar 'F' >>. pint64 |>> (int >> GoForward)

    let commandP =
        northP
        <|> southP
        <|> westP
        <|> eastP
        <|> leftP
        <|> rightP
        <|> forwardP

    let commandListP = sepEndBy1 commandP newline

    let commands =
        "day12"
        |> Data.readFile
        |> Data.parse commandListP

    let repeatedly func times =
        List.fold (>>) id (List.replicate times func)

    module Part1 =
        type Bearing =
            | East
            | West
            | North
            | South

        let turnLeft =
            function
            | East -> North
            | North -> West
            | West -> South
            | South -> East

        let turnRight =
            function
            | East -> South
            | South -> West
            | West -> North
            | North -> East

        type State = { X: int; Y: int; Bearing: Bearing }
        let initialState = { X = 0; Y = 0; Bearing = East }

        let rec advance state x =
            match state.Bearing with
            | North -> interpret state (GoNorth x)
            | East -> interpret state (GoEast x)
            | South -> interpret state (GoSouth x)
            | West -> interpret state (GoWest x)

        and interpret state cmd =
            match cmd with
            | GoNorth n -> { state with Y = state.Y + n }
            | GoEast e -> { state with X = state.X + e }
            | GoWest w -> { state with X = state.X - w }
            | GoSouth s -> { state with Y = state.Y - s }
            | TurnLeft l ->
                { state with
                      Bearing = repeatedly turnLeft (l / 90) state.Bearing }
            | TurnRight r ->
                { state with
                      Bearing = repeatedly turnRight (r / 90) state.Bearing }
            | GoForward f -> advance state f

        let answer =
            lazy
                (commands
                 |> List.fold interpret initialState
                 |> fun { X = x; Y = y } -> abs x + abs y)

    module Part2 =
        type State = { X: int; Y: int; DX: int; DY: int }
        let initialState = { X = 0; Y = 0; DX = 10; DY = 1 }

        let moveToWaypoint state =
            { state with
                  X = state.X + state.DX
                  Y = state.Y + state.DY }

        let transform a b c d state =
            { state with
                  DX = state.DX * a + state.DY * b
                  DY = state.DX * c + state.DY * d }

        let rotateWaypointLeft = transform 0 -1 1 0

        let rotateWaypointRight = transform 0 1 -1 0

        let interpret state (cmd: Command) =
            match cmd with
            | GoNorth n -> { state with DY = state.DY + n }
            | GoEast e -> { state with DX = state.DX + e }
            | GoWest w -> { state with DX = state.DX - w }
            | GoSouth s -> { state with DY = state.DY - s }
            | TurnLeft l -> repeatedly rotateWaypointLeft (l / 90) state
            | TurnRight r -> repeatedly rotateWaypointRight (r / 90) state
            | GoForward f -> repeatedly moveToWaypoint f state

        let answer =
            lazy
                (commands
                 |> List.fold interpret initialState
                 |> fun { X = x; Y = y } -> abs x + abs y)
