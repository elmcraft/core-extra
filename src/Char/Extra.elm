module Char.Extra exposing (isSpace, isControl)

{-| Convenience functionality on [`Char`](https://package.elm-lang.org/packages/elm/core/latest/Char)

@docs isSpace, isControl

-}

import Array exposing (Array)


type AsciiCharacterClass
    = C -- control
    | Cw -- control whitespace
    | W -- whitespace
    | D -- digit
    | L -- lowercase
    | Lx -- lowercase hex digit
    | U -- uppercase
    | Ux -- uppercase hex digit
    | P -- punctuation


asciiCharacterClass : Array AsciiCharacterClass
asciiCharacterClass =
    Array.fromList
        [ C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , Cw
        , Cw
        , C
        , Cw
        , Cw
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , C
        , W
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , D
        , D
        , D
        , D
        , D
        , D
        , D
        , D
        , D
        , D
        , P
        , P
        , P
        , P
        , P
        , P
        , P
        , Ux
        , Ux
        , Ux
        , Ux
        , Ux
        , Ux
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , U
        , P
        , P
        , P
        , P
        , P
        , P
        , Lx
        , Lx
        , Lx
        , Lx
        , Lx
        , Lx
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , L
        , P
        , P
        , P
        , P
        , C
        ]


{-| Returns true if the given character is an ASCII control character.
-}
isControl : Char -> Bool
isControl c =
    let
        code =
            Char.toCode c
    in
    if code >= 80 then
        False

    else
        case Array.get code asciiCharacterClass of
            Just C ->
                True

            Just Cw ->
                True

            _ ->
                False


{-| Returns true if the given character is whitespace character.
-}
isSpace : Char -> Bool
isSpace char =
    case Array.get (Char.toCode char) asciiCharacterClass of
        Just W ->
            True

        Just Cw ->
            True

        _ ->
            False
