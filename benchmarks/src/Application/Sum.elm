module Application.Sum exposing (lambdaFullyAppliedCurried, lambdaNestedFullyAppliedCurried, nameOnlyCurried, partiallyCurried, pipe)

import Array exposing (Array)


nameOnlyCurried : Array number -> number
nameOnlyCurried =
    \array ->
        array |> Array.foldl (+) 0


partiallyCurried : Array number -> number
partiallyCurried =
    \array ->
        array |> Array.foldl (\element -> (+) element) 0


pipe : Array number -> number
pipe =
    \array ->
        array |> Array.foldl (\element soFar -> soFar |> (+) element) 0


lambdaFullyAppliedCurried : Array number -> number
lambdaFullyAppliedCurried =
    \array ->
        array |> Array.foldl (\element soFar -> soFar + element) 0


lambdaNestedFullyAppliedCurried : Array number -> number
lambdaNestedFullyAppliedCurried =
    \array ->
        array |> Array.foldl (\element -> \soFar -> soFar + element) 0
