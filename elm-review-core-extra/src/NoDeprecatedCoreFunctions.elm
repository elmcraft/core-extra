module NoDeprecatedCoreFunctions exposing (rule)

{-|

@docs rule

-}

import Changes
import Dict exposing (Dict)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
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
    { lookupTable : ModuleNameLookupTable, imports : Set ModuleName, lastLineOfImports : Int }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> { lookupTable = lookupTable, imports = Set.empty, lastLineOfImports = 1 })
        |> Rule.withModuleNameLookupTable


importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
importVisitor node ctx =
    ( [], { ctx | imports = Set.insert (Node.value node |> .moduleName |> Node.value) ctx.imports, lastLineOfImports = max (Node.range node |> .end |> .row) ctx.lastLineOfImports } )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node ctx =
    case Node.value node of
        FunctionOrValue _ str ->
            case
                Review.ModuleNameLookupTable.moduleNameFor ctx.lookupTable node
            of
                Just moduleName ->
                    case Dict.get ( moduleName, str ) renames of
                        Just ( newModuleName, newName ) ->
                            ( [ Rule.errorWithFix
                                    { message = "Module uses core-extra deprecated functions"
                                    , details = [ "These functions are deprecated and will be removed in the next release" ]
                                    }
                                    (Node.range node)
                                    (Fix.replaceRangeBy (Node.range node) (String.join "." newModuleName ++ "." ++ newName)
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
                            case Changes.customRename moduleName str of
                                Just ( newModuleName, replacement ) ->
                                    ( [ Rule.errorWithFix
                                            { message = "Module uses core-extra deprecated functions"
                                            , details = [ "These functions are deprecated and will be removed in the next release" ]
                                            }
                                            (Node.range node)
                                            (Fix.replaceRangeBy (Node.range node) replacement
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

        _ ->
            ( [], ctx )


renames : Dict ( List String, String ) ( List String, String )
renames =
    Changes.renames
        |> List.map
            (\( from, to ) ->
                ( normalize from, normalize to )
            )
        |> Dict.fromList


normalize : String -> ( List String, String )
normalize name =
    case String.split "." name |> List.reverse of
        [] ->
            ( [], name )

        x :: xs ->
            ( List.reverse xs, x )
