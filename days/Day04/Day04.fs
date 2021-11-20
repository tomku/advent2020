namespace Advent

open FParsec

module Day04 =
    let fieldP = many1Chars (noneOf " \n\t:") .>> skipChar ':'
    let valueP = many1Chars (noneOf " \n\t") .>> skipAnyOf " \n\t"
    let recordP =  tuple2 fieldP valueP

    let passportP = many1 recordP |>> Map.ofList

    let passportListP = sepBy1 passportP newline

    let ParsePassports str =
        match run passportListP str with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let passports = Data.ReadFile("day04") |> ParsePassports

    module Part1 = 
        let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

        let valid passport =
            requiredFields |> List.except (Map.keys passport) |> List.isEmpty

        let answer = lazy ( passports |> List.filter valid |> List.length )

    module Part2 = 
        let inRangeInclusive label min max n =
            if n >= min && n <= max
            then preturn n
            else fail (sprintf "%s: %d not in range [%d,%d]" label n min max)

        let birthYearV = pint64 .>> followedBy eof >>= inRangeInclusive "byr" 1920 2002
        let issueYearV = pint64 .>> followedBy eof >>= inRangeInclusive "iyr" 2010 2020
        let expYearV =   pint64 .>> followedBy eof >>= inRangeInclusive "eyr" 2020 2030

        let cmV = pint64 .>>? pstring "cm" .>> followedBy eof >>= inRangeInclusive "hgt in cm" 150 193
        let inV = pint64 .>>? pstring "in" .>> followedBy eof >>= inRangeInclusive "hgt in in" 59 76
        let unitlessV = pint64 >>. fail "hgt not followed by 'in' or 'cm'"
        let heightV = (cmV <|> inV <|> unitlessV)
        
        let hexDigit = anyOf "0123456789abcdef"
        let hairV = pchar '#' >>. parray 6 hexDigit .>> followedBy eof <?> "hcl must start with '#' and be exactly six hex digits"

        let eyeColors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
        let eyeV = choice(List.map pstring eyeColors) .>> followedBy eof <?> "ecl must be one of: amb blu brn gry grn hzl oth"

        let pidV = parray 9 digit .>> followedBy eof <?> "pid must be exactly nine decimal digits"

        let validateField key parser pass = 
            Map.tryFind key pass
            |> Option.map(run parser)
            |> (function
                | Some(Success (_, _, _)) -> Result.Ok pass
                | Some(Failure (err, _, _)) -> Result.Error (pass, err)
                | None -> Result.Error (pass, ("required field " + key + " missing!"))
            )

        let validate  =
            let (>=>) l r = l >> Result.bind r

            validateField "byr" birthYearV 
            >=> validateField "iyr" issueYearV
            >=> validateField "eyr" expYearV
            >=> validateField "hgt" heightV
            >=> validateField "hcl" hairV
            >=> validateField "ecl" eyeV
            >=> validateField "pid" pidV

        let valid = function
            | Result.Ok p ->
                true
            | Result.Error e -> 
                false

        let evaluate =
            validate >> function
            | Result.Ok v ->
                printfn "Valid: %A" v
                Result.Ok v
            | Result.Error (pass, err) ->
                printfn "Invalid: %A\nReason: %A" pass err
                Result.Error err

        let answer = lazy ( passports |> List.map validate |> List.filter valid |> List.length )