namespace Advent

open FParsec

module Day04 =
    let fieldP =
        many1Chars (noneOf " \n\t:") .>> skipChar ':'

    let valueP =
        many1Chars (noneOf " \n\t") .>> skipAnyOf " \n\t"

    let recordP = tuple2 fieldP valueP

    let passportP = many1 recordP |>> Map.ofList

    let passportListP = sepBy1 passportP newline

    let parsePassports str =
        match run passportListP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let passports =
        Data.readFile ("day04") |> parsePassports

    module Part1 =
        let requiredFields =
            [ "byr"
              "iyr"
              "eyr"
              "hgt"
              "hcl"
              "ecl"
              "pid" ]

        let valid passport =
            requiredFields
            |> List.except (Map.keys passport)
            |> List.isEmpty

        let answer =
            lazy (passports |> List.filter valid |> List.length)

    module Part2 =
        let inRangeInclusive label min max n =
            if n >= min && n <= max then
                preturn n
            else
                fail (sprintf "%s: %d not in range [%d,%d]" label n min max)

        let birthYearV =
            pint64 .>> followedBy eof
            >>= inRangeInclusive "byr" 1920 2002

        let issueYearV =
            pint64 .>> followedBy eof
            >>= inRangeInclusive "iyr" 2010 2020

        let expYearV =
            pint64 .>> followedBy eof
            >>= inRangeInclusive "eyr" 2020 2030

        type Height =
            | Centimeters of int64
            | Inches of int64

        let cmV =
            pint64 .>>? pstring "cm" .>> followedBy eof
            >>= inRangeInclusive "hgt in cm" 150 193
            |>> Centimeters

        let inV =
            pint64 .>>? pstring "in" .>> followedBy eof
            >>= inRangeInclusive "hgt in in" 59 76
            |>> Inches

        let unitlessV =
            pint64 >>. fail "hgt not followed by 'in' or 'cm'"

        let heightV = (cmV <|> inV <|> unitlessV)

        type HairColor =
            { Red: int16
              Blue: int16
              Green: int16 }

        let makeHairColor r g b = { Red = r; Blue = b; Green = g }

        let hairV =
            let byte =
                manyMinMaxSatisfy 2 2 isHex
                |>> fun s -> System.Convert.ToInt16(s, 16)

            pchar '#' >>. pipe3 byte byte byte makeHairColor
            .>> followedBy eof
            <?> "hcl must start with '#' and be exactly six hex digits"

        type EyeColor =
            | Amber
            | Blue
            | Brown
            | Gray
            | Green
            | Hazel
            | Other

        let makeEyeColor =
            function
            | "amb" -> Amber
            | "blu" -> Blue
            | "brn" -> Brown
            | "gry" -> Gray
            | "grn" -> Green
            | "hzl" -> Hazel
            | "oth" -> Other
            | _ -> failwith "Unknown hair color!"

        let eyeColors =
            [ "amb"
              "blu"
              "brn"
              "gry"
              "grn"
              "hzl"
              "oth" ]

        let eyeV =
            choice (List.map pstring eyeColors)
            .>> followedBy eof
            <?> "ecl must be one of: amb blu brn gry grn hzl oth"
            |>> makeEyeColor

        let pidV =
            manyMinMaxSatisfy 9 9 isDigit .>> followedBy eof
            <?> "pid must be exactly nine decimal digits"

        let validateField key parser pass =
            Map.tryFind key pass
            |> Option.map (run parser)
            |> (function
            | Some (Success (_, _, _)) -> Result.Ok pass
            | Some (Failure (err, _, _)) -> Result.Error(pass, err)
            | None -> Result.Error(pass, ("required field " + key + " missing!")))

        let validate =
            let (>=>) l r = l >> Result.bind r

            validateField "byr" birthYearV
            >=> validateField "iyr" issueYearV
            >=> validateField "eyr" expYearV
            >=> validateField "hgt" heightV
            >=> validateField "hcl" hairV
            >=> validateField "ecl" eyeV
            >=> validateField "pid" pidV

        let valid =
            function
            | Result.Ok p -> [ p ]
            | Result.Error e -> []

        let evaluate =
            validate
            >> function
                | Result.Ok v ->
                    printfn "Valid: %A" v
                    Result.Ok v
                | Result.Error (pass, err) ->
                    printfn "Invalid: %A\nReason: %A" pass err
                    Result.Error err

        type Passport =
            { BirthYear: int64
              IssuedYear: int64
              ExpirationYear: int64
              Height: Height
              HairColor: HairColor
              EyeColor: EyeColor
              PassportID: string }

        let makePassport data =
            let unsafeParse p str =
                match run p str with
                | Success (x, _, _) -> x
                | Failure (e, _, _) -> failwith e

            { BirthYear = data |> Map.find "byr" |> unsafeParse birthYearV
              IssuedYear = data |> Map.find "iyr" |> unsafeParse issueYearV
              ExpirationYear = data |> Map.find "eyr" |> unsafeParse expYearV
              Height = data |> Map.find "hgt" |> unsafeParse heightV
              HairColor = data |> Map.find "hcl" |> unsafeParse hairV
              EyeColor = data |> Map.find "ecl" |> unsafeParse eyeV
              PassportID = data |> Map.find "pid" |> unsafeParse pidV }

        let validPassports =
            lazy
                (passports
                 |> List.map validate
                 |> List.map (Result.map makePassport)
                 |> List.collect valid)

        let answer =
            lazy (validPassports.Value |> List.length)
