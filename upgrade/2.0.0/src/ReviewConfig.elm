module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import Upgrade exposing (Upgrade)


config : List Rule
config =
    [ Upgrade.rule
        [ v1To2
        ]
    ]


v1To2 : Upgrade
v1To2 =
    [ Upgrade.application
        { oldName = ( "Array.Extra", "apply" )
        , oldArgumentNames = [ "functions", "arguments" ]
        , oldArgumentsToNew =
            \oldArguments ->
                case oldArguments of
                    [ functions, arguments ] ->
                        Upgrade.call ( "Array.Extra", "andMap" ) [ arguments, functions ]
                            |> Just

                    _ ->
                        Nothing
        }
    , Upgrade.application
        { oldName = ( "Array.Extra", "interweave" )
        , oldArgumentNames = [ "afterStart", "starting" ]
        , oldArgumentsToNew =
            \oldArguments ->
                case oldArguments of
                    [ afterStart, starting ] ->
                        Upgrade.call ( "Array.Extra", "interweave_" ) [ starting, afterStart ]
                            |> Just

                    _ ->
                        Nothing
        }
    , Upgrade.reference { old = ( "Basics.Extra", "swap" ), new = ( "Tuple.Extra", "flip" ) }
    , Upgrade.reference { old = ( "Basics.Extra", "atMost" ), new = ( "Basics", "min" ) }
    , Upgrade.reference { old = ( "Basics.Extra", "atLeast" ), new = ( "Basics", "max" ) }
    , Upgrade.reference { old = ( "Basics.Extra", "fractionalModBy" ), new = ( "Float.Extra", "modBy" ) }
    , Upgrade.reference { old = ( "Dict.Extra", "fromListDedupe" ), new = ( "Dict.Extra", "fromListCombining" ) }
    , Upgrade.reference { old = ( "Dict.Extra", "fromListDedupeBy" ), new = ( "Dict.Extra", "fromListByCombining" ) }
    , Upgrade.reference { old = ( "Dict.Extra", "insertDedupe" ), new = ( "Dict.Extra", "insertCombining" ) }
    , Upgrade.reference { old = ( "List.Extra", "filterNot" ), new = ( "List.Extra", "removeWhen" ) }
    , Upgrade.reference { old = ( "Maybe.Extra", "traverse" ), new = ( "Maybe.Extra", "combineMap" ) }
    , Upgrade.reference { old = ( "Maybe.Extra", "traverseArray" ), new = ( "Maybe.Extra", "combineMapArray" ) }
    , Upgrade.reference { old = ( "Result.Extra", "singleton" ), new = ( "Result", "Ok" ) }
    , Upgrade.reference { old = ( "String.Extra", "removeAccents" ), new = ( "String.Extra", "removeDiacritics" ) }
    , Upgrade.application
        { oldName = ( "Set.Extra", "subset" )
        , oldArgumentNames = [ "set1", "set2" ]
        , oldArgumentsToNew =
            \oldArguments ->
                case oldArguments of
                    [ set1, set2 ] ->
                        Upgrade.call ( "Set.Extra", "isSubsetOf" ) [ set2, set1 ]
                            |> Just

                    _ ->
                        Nothing
        }
    , Upgrade.reference { old = ( "Tuple.Extra", "sequenceMaybe" ), new = ( "Maybe.Extra", "combineBoth" ) }
    , Upgrade.reference { old = ( "Tuple.Extra", "sequenceFirstMaybe" ), new = ( "Maybe.Extra", "combineFirst" ) }
    , Upgrade.reference { old = ( "Tuple.Extra", "sequenceSecondMaybe" ), new = ( "Maybe.Extra", "combineSecond" ) }
    ]
        |> Upgrade.batch
