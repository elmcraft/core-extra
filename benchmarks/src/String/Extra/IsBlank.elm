module String.Extra.IsBlank exposing (regexBased, regexBasedWithTopLevelRegex, trimBased)

import Regex exposing (Regex)


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never


regexBased : String -> Bool
regexBased string =
    Regex.contains (regexFromString "^\\s*$") string


isBlankRegex : Regex
isBlankRegex =
    regexFromString "^\\s*$"


regexBasedWithTopLevelRegex : String -> Bool
regexBasedWithTopLevelRegex string =
    Regex.contains isBlankRegex string


trimBased : String -> Bool
trimBased string =
    String.trim string == ""
