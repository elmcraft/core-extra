module Changes exposing (..)


type Refactoring
    = BasicRename String
    | CustomRename (List String) String
    | RenameAndReorderArguments String (List Int)


refactors : List ( String, Refactoring )
refactors =
    [ ( "Array.Extra.apply", RenameAndReorderArguments "Array.Extra.andMap" [ 1, 0 ] )
    , ( "Array.Extra.interweave", RenameAndReorderArguments "Array.Extra.interweave_" [ 1, 0 ] )
    , ( "Basics.Extra.swap", BasicRename "Tuple.Extra.flip" )
    , ( "Basics.Extra.atMost", BasicRename "min" )
    , ( "Basics.Extra.atLeast", BasicRename "max" )
    , ( "Basics.Extra.fractionalModBy", BasicRename "Float.Extra.modBy" )
    , ( "Basics.Extra.orderBy", BasicRename "Order.Extra.breakTies" )
    , ( "Basics.Extra.toOrder", BasicRename "Order.Extra.byField" )
    , ( "Basics.Extra.toOrderDesc", CustomRename [ "Order", "Extra" ] "(Order.Extra.byField >> Order.Extra.reverse)" )
    , ( "Dict.Extra.fromListDedupe", BasicRename "Dict.Extra.fromListCombining" )
    , ( "Dict.Extra.fromListDedupeBy", BasicRename "Dict.Extra.fromListByCombining" )
    , ( "List.Extra.filterNot", BasicRename "List.Extra.removeWhen" )
    , ( "Maybe.Extra.traverse", BasicRename "Maybe.Extra.combineMap" )
    , ( "Maybe.Extra.traverseArray", BasicRename "Maybe.Extra.combineMapArray" )
    , ( "Result.Extra.singleton", BasicRename "Ok" )
    , ( "String.Extra.removeAccents", BasicRename "String.Extra.removeDiacritics" )
    , ( "Tuple.Extra.sequenceMaybe", BasicRename "Maybe.Extra.combineBoth" )
    , ( "Tuple.Extra.sequenceFirstMaybe", BasicRename "Maybe.Extra.combineFirst" )
    , ( "Tuple.Extra.sequenceSecondMaybe", BasicRename "Maybe.Extra.combineSecond" )
    ]
