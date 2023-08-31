module Benchmarks exposing (main)

import Application.NegAbs
import Application.Sum
import Array exposing (Array)
import Array.Extra as Array
import Array.Extra.All
import Array.Extra.Any
import Array.Extra.FilterMap
import Array.Extra.IndexedMapToList
import Array.Extra.Intersperse
import Array.Extra.Map2
import Array.Extra.MapToList
import Array.Extra.Member
import Array.Extra.Reverse
import Array.Extra.Unzip
import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (rank)
import Benchmark.Runner.Alternative as BenchmarkRunner


main : BenchmarkRunner.Program
main =
    describe "for array-extra"
        [ application
        , array
        , arrayExtra
        ]
        |> BenchmarkRunner.program


application : Benchmark
application =
    describe "application"
        [ rank "curry"
            (\sum -> ints1To100 |> sum)
            [ ( "name only", Application.Sum.nameOnlyCurried )
            , ( "partially curried/applied", Application.Sum.partiallyCurried )
            , ( "lambda, piping", Application.Sum.pipe )
            , ( "lambda, fully applied", Application.Sum.lambdaFullyAppliedCurried )
            , ( "lambda nested, fully applied", Application.Sum.lambdaNestedFullyAppliedCurried )
            ]
        , rank "chain"
            (\negAbs -> ints1To100 |> Array.map negAbs)
            [ ( "declaration argument, |> |>", Application.NegAbs.declarationArgumentPipeline )
            , ( "lambda, |> |>", Application.NegAbs.lambdaPipeline )
            , ( "lambda, |> >>", Application.NegAbs.lambdaPipeComposeR )
            , ( ">>", Application.NegAbs.composeR )
            ]
        ]


array : Benchmark
array =
    describe "Array"
        [ rank "Array.fold"
            (\fold -> ints1To100 |> fold (+) 0)
            [ ( "foldl", Array.foldl )
            , ( "foldr", Array.foldr )
            ]
        ]


arrayExtra : Benchmark
arrayExtra =
    describe "Array.Extra"
        [ rank "mapToList"
            (\mapToList -> ints1To100 |> mapToList negate)
            [ ( "with foldr", Array.Extra.MapToList.withFoldr )
            , ( "with Array.toIndexedList", Array.Extra.MapToList.withListMap )
            ]
        , rank "indexedMapToList"
            (\indexedMapToList ->
                ints1To100 |> indexedMapToList Tuple.pair
            )
            [ ( "with Array.foldr", Array.Extra.IndexedMapToList.withFoldr )
            , ( "with toIndexedList"
              , Array.Extra.IndexedMapToList.withToIndexedList
              )
            , ( "with Array.indexedMap"
              , Array.Extra.IndexedMapToList.withArrayIndexedMap
              )
            , ( "with List.indexedMap"
              , Array.Extra.IndexedMapToList.withListIndexedMap
              )
            ]
        , rank "reverse"
            (\reverse -> reverse ints1To100)
            [ ( "with cons", Array.Extra.Reverse.withCons )
            , ( "with List.reverse", Array.Extra.Reverse.withListReverse )
            , ( "with push", Array.Extra.Reverse.withPush )
            ]
        , let
            zipped =
                Array.zip ints1To100 ints1To100
          in
          rank "unzip"
            (\unzip -> zipped |> unzip)
            [ ( "with maps", Array.Extra.Unzip.withMaps )
            , ( "with List.unzip", Array.Extra.Unzip.withListUnzip )
            , ( "with push", Array.Extra.Unzip.wthPush )
            , ( "with cons", Array.Extra.Unzip.wthCons )
            ]
        , rank "map2"
            (\map2 ->
                map2 Tuple.pair ints1To100 ints1To100
            )
            [ ( "with List.map2", Array.Extra.Map2.withListMap2 )
            , ( "with get", Array.Extra.Map2.withGet )
            , ( "with un-cons", Array.Extra.Map2.withUncons )
            ]
        , let
            maybeInts =
                Array.initialize 100
                    (\x ->
                        if (x |> modBy 3) == 0 then
                            Nothing

                        else
                            Just x
                    )
          in
          rank "filterMap"
            (\filterMap -> maybeInts |> filterMap identity)
            [ ( "with List.filterMap", Array.Extra.FilterMap.withListFilterMap )
            , ( "with push", Array.Extra.FilterMap.withPush )
            , ( "with cons", Array.Extra.FilterMap.withCons )
            ]
        , let
            allTrue =
                Array.repeat 100 True
          in
          rank "all"
            (\all -> allTrue |> all identity)
            [ ( "recursive last", Array.Extra.All.recursiveLast )
            , ( "recursive get", Array.Extra.All.recursiveGet )
            , ( "with List.all", Array.Extra.All.withListAll )
            , ( "with fold", Array.Extra.All.withFold )
            ]
        , let
            allFalse =
                Array.repeat 100 False
          in
          rank "any"
            (\any -> allFalse |> any identity)
            [ ( "recursive last", Array.Extra.Any.recursiveLast )
            , ( "recursive get", Array.Extra.Any.recursiveGet )
            , ( "with List.any", Array.Extra.Any.withList )
            , ( "with fold", Array.Extra.Any.withFold )
            ]
        , rank "intersperse"
            (\intersperse -> ints1To100 |> intersperse 0)
            [ ( "with push", Array.Extra.Intersperse.withPush )
            , ( "with cons", Array.Extra.Intersperse.withCons )
            , ( "with List.intersperse", Array.Extra.Intersperse.withListIntersperse )
            ]
        , rank "member"
            (\member -> member 50 ints1To100)
            [ ( "with fold", Array.Extra.Member.withFold )
            , ( "recursive", Array.Extra.Member.recursive )
            , ( "with List.member", Array.Extra.Member.withList )
            , ( "with any", Array.Extra.Member.withAny )
            ]
        ]


ints1To100 : Array Int
ints1To100 =
    Array.fromList (List.range 1 100)
