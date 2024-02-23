module Set.Extra.AreDisjoint exposing (foldl, foldr, intersection, listRecursion)

import Set


intersection a b =
    Set.intersect a b
        |> Set.isEmpty


listRecursion a b =
    listRecursionHelper (Set.toList a) b


listRecursionHelper list set =
    case list of
        [] ->
            True

        x :: xs ->
            if Set.member x set then
                False

            else
                listRecursionHelper xs set


foldr a b =
    not (Set.foldr (\x so -> so || Set.member x b) False a)


foldl a b =
    not (Set.foldl (\x so -> so || Set.member x b) False a)
