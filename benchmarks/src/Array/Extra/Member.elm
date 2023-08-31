module Array.Extra.Member exposing (recursive, recursiveFromIndex, withAny, withFold, withList)

import Array exposing (Array)
import Array.Extra


withFold : a -> Array a -> Bool
withFold needle =
    \array ->
        array
            |> Array.foldl (\el soFar -> soFar || needle == el) False


recursive : a -> Array a -> Bool
recursive needle =
    \array ->
        array |> recursiveFromIndex 0 needle


recursiveFromIndex : Int -> a -> Array a -> Bool
recursiveFromIndex index needle =
    \array ->
        case array |> Array.get index of
            Just atIndex ->
                if atIndex == needle then
                    True

                else
                    array |> recursiveFromIndex (index + 1) needle

            Nothing ->
                False


withList : a -> Array a -> Bool
withList needle =
    \array ->
        array |> Array.toList |> List.member needle


withAny : a -> Array a -> Bool
withAny needle =
    \array ->
        array |> Array.Extra.any (\element -> element == needle)
