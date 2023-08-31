module Array.Extra.Map2 exposing (withGet, withListMap2, withUncons)

import Array exposing (Array)
import Array.Extra
import Html exposing (b)


withListMap2 : (a -> b -> c) -> Array a -> Array b -> Array c
withListMap2 combine aArray bArray =
    List.map2 combine
        (aArray |> Array.toList)
        (bArray |> Array.toList)
        |> Array.fromList


withGet : (a -> b -> c) -> Array a -> Array b -> Array c
withGet combine aArray bArray =
    aArray
        |> Array.foldl
            (\aElement state ->
                if state.bsExhausted then
                    state

                else
                    case bArray |> Array.get state.index of
                        Just bElement ->
                            { state
                                | combined =
                                    state.combined
                                        |> (::) (combine aElement bElement)
                                , index = state.index + 1
                            }

                        Nothing ->
                            { state | bsExhausted = True }
            )
            { bsExhausted = False
            , index = 0
            , combined = []
            }
        |> .combined
        |> List.reverse
        |> Array.fromList


withUncons : (a -> b -> c) -> Array a -> Array b -> Array c
withUncons combine aArray bArray =
    aArray
        |> Array.foldl
            (\aElement state ->
                case state.bsRemaining of
                    [] ->
                        state

                    bsRemainingHead :: bsRemainingTail ->
                        { combined =
                            state.combined
                                |> (::) (combine aElement bsRemainingHead)
                        , bsRemaining = bsRemainingTail
                        }
            )
            { bsRemaining = bArray |> Array.toList
            , combined = []
            }
        |> .combined
        |> List.reverse
        |> Array.fromList
