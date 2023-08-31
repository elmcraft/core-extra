module Array.Extra.Unzip exposing (withListUnzip, withMaps, wthCons, wthPush)

import Array exposing (Array)


withMaps : Array ( a, b ) -> ( Array a, Array b )
withMaps =
    \arrayOfTuple ->
        ( arrayOfTuple |> Array.map Tuple.first
        , arrayOfTuple |> Array.map Tuple.second
        )


withListUnzip : Array ( a, b ) -> ( Array a, Array b )
withListUnzip =
    \arrayOfTuple ->
        arrayOfTuple
            |> Array.toList
            |> List.unzip
            |> Tuple.mapBoth Array.fromList Array.fromList


wthPush : Array ( a, b ) -> ( Array a, Array b )
wthPush =
    \arrayOfTuple ->
        arrayOfTuple
            |> Array.foldl
                (\( a, b ) ( arrayOfASoFar, arrayOfBSoFar ) ->
                    ( arrayOfASoFar |> Array.push a
                    , arrayOfBSoFar |> Array.push b
                    )
                )
                ( Array.empty, Array.empty )


wthCons : Array ( a, b ) -> ( Array a, Array b )
wthCons =
    \arrayOfTuple ->
        arrayOfTuple
            |> Array.foldr
                (\( a, b ) ( arrayOfASoFar, arrayOfBSoFar ) ->
                    ( arrayOfASoFar |> (::) a
                    , arrayOfBSoFar |> (::) b
                    )
                )
                ( [], [] )
            |> Tuple.mapBoth Array.fromList Array.fromList
