module Array.Extra.Intersperse exposing (withCons, withListIntersperse, withPush)

import Array exposing (Array)
import Array.Extra


withPush : a -> Array a -> Array a
withPush separator =
    \array ->
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                Array.empty

            Just last ->
                let
                    step element =
                        \beforeStep ->
                            beforeStep
                                |> Array.push element
                                |> Array.push separator

                    beforeLastInterspersed =
                        array
                            |> Array.Extra.pop
                            |> Array.foldr step Array.empty
                in
                beforeLastInterspersed |> Array.push last


withCons : a -> Array a -> Array a
withCons separator =
    \array ->
        case array |> Array.get ((array |> Array.length) - 1) of
            Nothing ->
                Array.empty

            Just last ->
                let
                    step element =
                        \beforeStep ->
                            beforeStep
                                |> (::) element
                                |> (::) separator

                    beforeLastInterspersed =
                        array
                            |> Array.Extra.pop
                            |> Array.foldl step []
                in
                beforeLastInterspersed |> (::) last |> List.reverse |> Array.fromList


withListIntersperse : a -> Array a -> Array a
withListIntersperse separator =
    \array ->
        array
            |> Array.toList
            |> List.intersperse separator
            |> Array.fromList
