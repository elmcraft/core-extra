module List.Extra.InsertAt exposing
    ( insertAtRecursion
    , insertAtRecursion2
    , insertAtRecursion3
    , insertAtSplitAt
    , insertAtTakeDrop
    )

import List.Extra


insertAtRecursion : Int -> a -> List a -> List a
insertAtRecursion index value list =
    if index <= -1 then
        list

    else
        let
            go : Int -> List a -> List a -> List a
            go i rest acc =
                if i == index then
                    List.reverse acc ++ (value :: rest)

                else
                    case rest of
                        [] ->
                            -- index > length list
                            list

                        head :: newRest ->
                            go (i + 1) newRest (head :: acc)
        in
        go 0 list []


insertAtRecursion2Help : Int -> a -> List a -> Int -> List a -> List a -> List a
insertAtRecursion2Help index value list i rest acc =
    if i == index then
        List.foldl (::) (value :: rest) acc

    else
        case rest of
            [] ->
                -- index > length list
                list

            head :: newRest ->
                insertAtRecursion2Help index value list (i + 1) newRest (head :: acc)


insertAtRecursion2 : Int -> a -> List a -> List a
insertAtRecursion2 index value list =
    if index <= -1 then
        list

    else
        insertAtRecursion2Help index value list 0 list []


insertAtTakeDrop : Int -> a -> List a -> List a
insertAtTakeDrop index value list =
    if index <= -1 then
        list

    else
        let
            length =
                List.length list
        in
        if length < index then
            list

        else
            List.take index list ++ (value :: List.drop index list)


insertAtSplitAt : Int -> a -> List a -> List a
insertAtSplitAt index value list =
    if index <= -1 then
        list

    else
        let
            ( before, after ) =
                List.Extra.splitAt index list
        in
        if List.isEmpty after && List.length before < index then
            list

        else
            before ++ (value :: after)


insertAtRecursion3Help : a -> List a -> Int -> List a -> List a -> List a
insertAtRecursion3Help value list i rest acc =
    if i == 0 then
        List.foldl (::) (value :: rest) acc

    else
        case rest of
            [] ->
                -- index > length list
                list

            head :: newRest ->
                insertAtRecursion3Help value list (i - 1) newRest (head :: acc)


insertAtRecursion3 : Int -> a -> List a -> List a
insertAtRecursion3 index value list =
    if index <= -1 then
        list

    else
        insertAtRecursion3Help value list index list []
