module List.Extra.UniquePairs exposing (originalConcat, tailRecursive)

import List.Extra exposing (reverseMap)


originalConcat : List a -> List ( a, a )
originalConcat xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            List.map (\y -> ( x, y )) xs_ ++ originalConcat xs_


tailRecursive : List a -> List ( a, a )
tailRecursive xs =
    tailRecursiveHelp xs []


tailRecursiveHelp : List a -> List (List ( a, a )) -> List ( a, a )
tailRecursiveHelp xs soFar =
    case xs of
        [] ->
            List.foldl List.append [] soFar

        x :: xs_ ->
            tailRecursiveHelp xs_ (reverseMap (\y -> ( x, y )) xs_ :: soFar)
