module NoDeprecatedCoreFunctions exposing (rule)

{-|

@docs rule

-}

import Array
import Changes exposing (Refactoring(..))
import Dict exposing (Dict)
import Elm.Pretty
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range, compareLocations)
import Pretty
import Review.Fix as Fix
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Upgrades deprecated functions for their newer versions.
-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoDeprecatedCoreFunctions" initialContext
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , imports : Set ModuleName
    , lastLineOfImports : Int
    , applyHandled : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> { lookupTable = lookupTable, imports = Set.empty, lastLineOfImports = 1, applyHandled = [] })
        |> Rule.withModuleNameLookupTable


importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor node ctx =
    ( [], { ctx | imports = Set.insert (Node.value node |> .moduleName |> Node.value) ctx.imports, lastLineOfImports = max (Node.range node |> .end |> .row) ctx.lastLineOfImports } )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node ctx =
    case Node.value node of
        FunctionOrValue modName str ->
            case
                Review.ModuleNameLookupTable.moduleNameFor ctx.lookupTable node
            of
                Just moduleName ->
                    let
                        myRange =
                            Node.range node
                    in
                    case Dict.get ( moduleName, str ) applications of
                        Just ( newModPath, newFunName, _ ) ->
                            if not (List.any (\range -> compareLocations range.start myRange.start /= GT && compareLocations range.end myRange.end /= LT) ctx.applyHandled) then
                                ( [ Rule.error
                                        { message = "Module uses core-extra deprecated functions"
                                        , details =
                                            [ denormalize modName str
                                                ++ " is deprecated in favour of "
                                                ++ denormalize
                                                    (if moduleName == newModPath then
                                                        modName

                                                     else
                                                        newModPath
                                                    )
                                                    newFunName
                                                ++ " and will be removed."
                                            , "However, we cannot automatically upgrade this usage for you, so please update this manually."
                                            , "Note that the argument order is different."
                                            ]
                                        }
                                        (Node.range node)
                                  ]
                                , ctx
                                )

                            else
                                ( [], ctx )

                        Nothing ->
                            case Dict.get ( moduleName, str ) renames of
                                Just ( newModuleName, newCode ) ->
                                    ( [ Rule.errorWithFix
                                            { message = "Module uses core-extra deprecated functions"
                                            , details = [ "These functions are deprecated and will be removed in the next release" ]
                                            }
                                            (Node.range node)
                                            (Fix.replaceRangeBy (Node.range node) newCode
                                                :: (if newModuleName == [] || Set.member newModuleName ctx.imports then
                                                        []

                                                    else
                                                        [ Fix.insertAt { row = ctx.lastLineOfImports + 1, column = 0 } ("import " ++ String.join "." newModuleName ++ "\n") ]
                                                   )
                                            )
                                      ]
                                    , ctx
                                    )

                                Nothing ->
                                    ( [], ctx )

                Nothing ->
                    ( [], ctx )

        Application (((Node _ (FunctionOrValue modName funName)) as subNode) :: argList) ->
            case
                Review.ModuleNameLookupTable.moduleNameFor ctx.lookupTable subNode
            of
                Just moduleName ->
                    case Dict.get ( moduleName, funName ) applications of
                        Just ( newModPath, newFunName, argIndices ) ->
                            case getMany argIndices (Array.fromList argList) of
                                Just newArgs ->
                                    ( [ Rule.errorWithFix
                                            { message = "Module uses core-extra deprecated functions"
                                            , details = [ "These functions are deprecated and will be removed in the next release" ]
                                            }
                                            (Node.range node)
                                            (Fix.replaceRangeBy (Node.range node)
                                                (Elm.Pretty.prettyExpression
                                                    (Application
                                                        (Node Elm.Syntax.Range.emptyRange
                                                            (FunctionOrValue
                                                                (if moduleName == newModPath then
                                                                    modName

                                                                 else
                                                                    newModPath
                                                                )
                                                                newFunName
                                                            )
                                                            :: newArgs
                                                        )
                                                    )
                                                    |> Pretty.pretty 120
                                                )
                                                :: (if newModPath == [] || Set.member newModPath ctx.imports then
                                                        []

                                                    else
                                                        [ Fix.insertAt { row = ctx.lastLineOfImports + 1, column = 0 } ("import " ++ String.join "." newModPath ++ "\n") ]
                                                   )
                                            )
                                      ]
                                    , { ctx | applyHandled = Node.range node :: ctx.applyHandled }
                                    )

                                Nothing ->
                                    ( [ Rule.error
                                            { message = "Module uses core-extra deprecated functions"
                                            , details =
                                                [ denormalize modName funName
                                                    ++ " is deprecated in favour of "
                                                    ++ denormalize
                                                        (if moduleName == newModPath then
                                                            modName

                                                         else
                                                            newModPath
                                                        )
                                                        newFunName
                                                    ++ " and will be removed."
                                                , "However, we cannot automatically upgrade this usage for you, so please update this manually."
                                                , "Note that the argument order is different."
                                                ]
                                            }
                                            (Node.range node)
                                      ]
                                    , ctx
                                    )

                        Nothing ->
                            ( [], ctx )

                _ ->
                    ( [], ctx )

        _ ->
            ( [], ctx )


renames : Dict ( List String, String ) ( List String, String )
renames =
    Changes.refactors
        |> List.filterMap
            (\( from, to ) ->
                case to of
                    BasicRename target ->
                        Just ( normalize from, ( Tuple.first (normalize target), target ) )

                    CustomRename modName target ->
                        Just ( normalize from, ( modName, target ) )

                    RenameAndReorderArguments _ _ ->
                        Nothing
            )
        |> Dict.fromList


applications : Dict ( List String, String ) ( List String, String, List Int )
applications =
    Changes.refactors
        |> List.filterMap
            (\( from, to ) ->
                case to of
                    BasicRename _ ->
                        Nothing

                    CustomRename _ _ ->
                        Nothing

                    RenameAndReorderArguments target args ->
                        let
                            ( mod, name ) =
                                normalize target
                        in
                        Just ( normalize from, ( mod, name, args ) )
            )
        |> Dict.fromList


normalize : String -> ( List String, String )
normalize name =
    case String.split "." name |> List.reverse of
        [] ->
            ( [], name )

        x :: xs ->
            ( List.reverse xs, x )


denormalize : List String -> String -> String
denormalize modPath name =
    String.join "." (List.reverse (name :: List.reverse modPath))


getMany : List Int -> Array.Array a -> Maybe (List a)
getMany indices array =
    List.foldr
        (\index ->
            Maybe.map2 (::) (Array.get index array)
        )
        (Just [])
        indices
