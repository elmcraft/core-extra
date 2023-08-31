module Array.Extra.MapToList exposing (withFoldr, withListMap)

import Array exposing (Array)


withFoldr : (a -> b) -> Array a -> List b
withFoldr alter =
    \array ->
        array
            |> Array.foldr
                (\element soFar ->
                    soFar |> (::) (element |> alter)
                )
                []


withListMap : (a -> b) -> Array a -> List b
withListMap alter =
    \array ->
        array |> Array.toList |> List.map alter
