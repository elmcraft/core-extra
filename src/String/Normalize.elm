module String.Normalize exposing
    ( url, slug, filename, path
    , removeDiacritics
    )

{-| Normalize string in different formats


# Normalize formats

@docs url, slug, filename, path


# Utilities

@docs removeDiacritics

-}

import Char
import Dict
import Set exposing (Set)
import String.Normalize.Diacritics as Diacritics
import Array


{-| `removeDiacritics` removes diactritics, it will expand
known ligatures, thus changing the string glyph length.
All non latin characters are untouched.

    removeDiacritics "La liberté commence où l'ignorance finit."


    --> "La liberte commence ou l'ignorance finit."
    removeDiacritics "é()/& abc" --> "e()/& abc"

    removeDiacritics "こんにちは" --> "こんにちは"

-}
removeDiacritics : String -> String
removeDiacritics str =
    let
        replace c result =
            if Char.toCode c < Diacritics.minCode then
                result ++ String.fromChar c

            else
                case
                    Array.get
                        (Char.toCode c)
                        Diacritics.lookupArray
                of
                    Just candidate ->
                        result ++ candidate

                    Nothing ->
                        result ++ String.fromChar c
    in
    String.foldl replace "" str


{-| `slug` will remove all diacritics and punctuation, lowercase the string,
collapse spaces and replace them with hyphens. Trailing and leading hyphens
will be removed.

    slug "Écoute la vie!" -> "ecoute-la-vie"
    slug "(this is not interesting!)" -> "this-is-not-interesting"
    slug "é()/& abc  -" --> "e-abc"

-}
slug : String -> String
slug str =
    cleanup Set.empty '-' str


{-| `url` is similar to `slug` but will preserve slashes.

    url "J'aime les fruits/et les amours" --> "j-aime-les-fruits/et-les-amours"

-}
url : String -> String
url str =
    cleanup (Set.singleton '/') '-' str


{-| `filename` is similar to `slug` but will use underscores
instead of hypens, it will also preserve the file extension. The file
extension will be lowercased.

    filename "J'aime les fruits/et les amours .MP3"
        -->
        "j_aime_les_fruits_et_les_amours.mp3"

-}
filename : String -> String
filename str =
    cleanupPath Set.empty str


{-| `path` is similar to `filename` but will preserve slash.

    path "J'aime les fruits/et les amours .MP3"
        -->
        "j_aime_les_fruits/et_les_amours.mp3"

-}
path : String -> String
path str =
    cleanupPath (Set.singleton '/') str


cleanupPath : Set Char -> String -> String
cleanupPath keepChars str =
    let
        separate c ( name, ext, found ) =
            let
                name_ =
                    if found then
                        String.fromChar c ++ name

                    else
                        name

                ext_ =
                    if not found then
                        String.fromChar c ++ ext

                    else
                        ext

                found_ =
                    if c == '.' then
                        True

                    else
                        found
            in
            ( name_, ext_, found_ )

        ( fname, rawExt, _ ) =
            String.foldr separate ( "", "", False ) str

        fname_ =
            cleanup keepChars '_' fname

        lowExt =
            String.toLower rawExt
    in
    fname_ ++ lowExt


cleanup : Set Char -> Char -> String -> String
cleanup keepChars separator str =
    let
        removePunctuation c =
            let
                code =
                    Char.toCode c

                newChar =
                    if Set.member c keepChars then
                        ( c, True, True )

                    else if code > 0x7F then
                        ( c, False, False )

                    else if
                        code
                            > 0x7A
                            || (code > 0x5A && code < 0x61)
                            || (code > 0x39 && code < 0x41)
                            || (code < 0x30)
                    then
                        ( '-', True, False )

                    else
                        ( c, False, False )
            in
            newChar

        replace c ( resultAcc, isPreviousBoundary, isPreviousKeep ) =
            case Dict.get c Diacritics.lookupTable of
                Just candidate ->
                    ( resultAcc ++ String.toLower candidate, False, False )

                Nothing ->
                    let
                        ( replacement, isBoundary, isKeep ) =
                            removePunctuation c

                        replacement_ =
                            replacement
                                |> String.fromChar
                                |> String.toLower
                    in
                    if isBoundary && isPreviousBoundary && isPreviousKeep then
                        ( resultAcc
                        , isPreviousBoundary
                        , isPreviousKeep
                        )

                    else if isBoundary && isPreviousBoundary then
                        ( String.dropRight 1 resultAcc ++ replacement_
                        , isBoundary
                        , isKeep
                        )

                    else
                        ( resultAcc ++ replacement_
                        , isBoundary
                        , isKeep
                        )

        ( result, _, _ ) =
            String.foldl replace ( "", True, False ) str

        result_ =
            if String.startsWith "-" result then
                String.dropLeft 1 result

            else
                result

        result__ =
            if String.endsWith "-" result_ then
                String.dropRight 1 result_

            else
                result_
    in
    result__
