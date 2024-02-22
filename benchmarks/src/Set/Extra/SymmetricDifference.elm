module Set.Extra.SymmetricDifference exposing (naive, orderExploiting)

import Set


naive a b =
    Set.diff (Set.union a b) (Set.intersect a b)


orderExploiting a b =
    orderExploitingHelp (Set.toList a) (Set.toList b) Set.empty


orderExploitingHelp a b soFar =
    case ( a, b ) of
        ( x :: xs, y :: ys ) ->
            case compare x y of
                EQ ->
                    orderExploitingHelp xs ys soFar

                GT ->
                    orderExploitingHelp a ys (Set.insert y soFar)

                LT ->
                    orderExploitingHelp xs b (Set.insert x soFar)

        _ ->
            Set.union (Set.union (Set.fromList a) (Set.fromList b)) soFar
