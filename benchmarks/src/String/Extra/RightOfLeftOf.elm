module String.Extra.RightOfLeftOf exposing (leftOfIndexes, leftOfRegex, rightOfIndexes, rightOfRegex)

import Regex exposing (Regex)


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


regexBased : String -> Bool
regexBased string =
    Regex.contains (regexFromString "^\\s*$") string


firstResult : List (Maybe String) -> String
firstResult list =
    firstResultHelp "" list


firstResultHelp : String -> List (Maybe String) -> String
firstResultHelp default list =
    case list of
        [] ->
            default

        (Just a) :: _ ->
            a

        Nothing :: rest ->
            firstResultHelp default rest


rightOfRegex : String -> String -> String
rightOfRegex pattern string =
    string
        |> Regex.findAtMost 1 (regexFromString <| regexEscape pattern ++ "((?:.|\\s)*)$")
        |> List.map (.submatches >> firstResult)
        |> String.concat


rightOfIndexes : String -> String -> String
rightOfIndexes pattern string =
    case String.indexes pattern string of
        [] ->
            ""

        firstIndex :: rest ->
            String.slice (String.length pattern + firstIndex) (String.length string) string


leftOfRegex : String -> String -> String
leftOfRegex pattern string =
    string
        |> Regex.findAtMost 1 (regexFromString <| "^((?:.|\\s)*?)" ++ regexEscape pattern)
        |> List.map (.submatches >> firstResult)
        |> String.concat


leftOfIndexes : String -> String -> String
leftOfIndexes pattern string =
    case String.indexes pattern string of
        [] ->
            ""

        firstIndex :: rest ->
            String.slice 0 firstIndex string


regexEscape : String -> String
regexEscape =
    Regex.replace regexEscapeRegex (\{ match } -> "\\" ++ match)


regexEscapeRegex : Regex
regexEscapeRegex =
    regexFromString "[-/\\^$*+?.()|[\\]{}]"
