module Array.Extra.Reverse exposing (withCons, withListReverse, withPush)

import Array exposing (Array)


withCons : Array a -> Array a
withCons =
    \array ->
        array |> Array.foldl (::) [] |> Array.fromList


withPush : Array a -> Array a
withPush =
    \array ->
        array |> Array.foldl Array.push Array.empty


withListReverse : Array a -> Array a
withListReverse =
    \array ->
        array |> Array.toList |> List.reverse |> Array.fromList
