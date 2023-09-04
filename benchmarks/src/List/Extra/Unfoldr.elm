module List.Extra.Unfoldr exposing (nonTailRecursive, tailRecursive)


nonTailRecursive : (b -> Maybe ( a, b )) -> b -> List a
nonTailRecursive f seed =
    case f seed of
        Nothing ->
            []

        Just ( a, b ) ->
            a :: nonTailRecursive f b


tailRecursive : (b -> Maybe ( a, b )) -> b -> List a
tailRecursive f seed =
    tailRecursiveHelp f seed []


tailRecursiveHelp : (b -> Maybe ( a, b )) -> b -> List a -> List a
tailRecursiveHelp f seed soFar =
    case f seed of
        Nothing ->
            List.reverse soFar

        Just ( a, b ) ->
            tailRecursiveHelp f b (a :: soFar)
