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
import List.Extra
import List.Extra.GroupsOf
import List.Extra.Lift
import List.Extra.Unfoldr
import List.Extra.UniquePairs
import Maybe.Extra.AndMap
import Set exposing (Set)
import Set.Extra.AreDisjoint
import Set.Extra.SymmetricDifference
import String.Extra.IsBlank


main : BenchmarkRunner.Program
main =
    describe "for core-extra"
        [ application
        , array
        , arrayExtra
        , listExtra
        , tupleExtra
        , setExtra
        , stringExtra
        , maybeExtra
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


listExtra : Benchmark
listExtra =
    let
        shortList =
            List.range 1 10

        intList =
            List.range 1 100
    in
    describe "List.Extra"
        ([ rank "uniquePairs"
            (\uniquePairs -> uniquePairs intList)
            [ ( "original (++)", List.Extra.UniquePairs.originalConcat )
            , ( "tail-recursive", List.Extra.UniquePairs.tailRecursive )
            ]
         , rank "unfoldr"
            (\unfoldr -> unfoldr subtractOneUntilZero 100)
            [ ( "original", List.Extra.Unfoldr.nonTailRecursive )
            , ( "tail-recursive", List.Extra.Unfoldr.tailRecursive )
            ]
         , rank "lift2"
            (\lift2 -> lift2 (\a b -> a + b) shortList shortList)
            [ ( "original", List.Extra.Lift.liftAndThen2 )
            , ( "foldl", List.Extra.Lift.liftFold2 )
            ]
         , rank "lift3"
            (\lift3 -> lift3 (\a b c -> a + b + c) shortList shortList shortList)
            [ ( "original", List.Extra.Lift.liftAndThen3 )
            , ( "foldl", List.Extra.Lift.liftFold3 )
            ]
         , rank "lift4"
            (\lift4 -> lift4 (\a b c d -> a + b + c + d) shortList shortList shortList shortList)
            [ ( "original", List.Extra.Lift.liftAndThen4 )
            , ( "foldl", List.Extra.Lift.liftFold4 )
            ]
         ]
            ++ List.concatMap toComparisonsGroupsOfWithStep (List.range 1 4)
        )


toComparisonsGroupsOfWithStep : Int -> List Benchmark
toComparisonsGroupsOfWithStep exponent =
    let
        listSize =
            10 ^ exponent

        range =
            List.range 1 listSize
    in
    [ rank ("groupsOfWithStep 3 2 [1.." ++ String.fromInt listSize ++ "]")
        (\impl -> impl 3 2 range)
        [ ( "using elm-core's List.tail", List.Extra.GroupsOf.coreTailGroupsOfWithStep )
        , ( "using fully tail-recursive List.tail", List.Extra.GroupsOf.tailRecGroupsOfWithStep )
        ]
    , rank ("greedyGroupsOfWithStep 3 2 [1.." ++ String.fromInt listSize ++ "]")
        (\impl -> impl 3 2 range)
        [ ( "using elm-core's List.tail", List.Extra.GroupsOf.coreTailGreedyGroupsOfWithStep )
        , ( "using fully tail-recursive List.tail", List.Extra.GroupsOf.tailRecGreedyGroupsOfWithStep )
        ]
    ]


tupleExtra : Benchmark
tupleExtra =
    describe "Tuple.Extra"
        [ Benchmark.compare "construction" "literal" (\() -> ( 1, "a" )) "function" (\() -> Tuple.pair 1 "a")
        ]


stringExtra : Benchmark
stringExtra =
    describe "String.Extra"
        [ stringExtraIsBlank
        ]


maybeExtra : Benchmark
maybeExtra =
    describe "Maybe.Extra"
        [ rank "andMap - Just × Just"
            (\andMap -> Just negate |> andMap (Just 0))
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        , rank "andMap - Nothing × Just"
            (\andMap -> Nothing |> andMap (Just 0))
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        , rank "andMap - Just × Nothing"
            (\andMap -> Just negate |> andMap Nothing)
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        , rank "andMap - Nothing × Nothing"
            (\andMap -> Nothing |> andMap Nothing)
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        ]


stringExtraIsBlank : Benchmark
stringExtraIsBlank =
    let
        bench label string =
            rank label
                (\isBlank -> isBlank string)
                [ ( "regex based", String.Extra.IsBlank.regexBased )
                , ( "regex based (with top-level regex)", String.Extra.IsBlank.regexBasedWithTopLevelRegex )
                , ( "trim based", String.Extra.IsBlank.trimBased )
                ]

        emptyString =
            ""

        wsString =
            String.repeat 10 " "

        fullString =
            String.repeat 10 "Hello World"
    in
    Benchmark.describe "isBlank"
        [ bench "empty string" emptyString
        , bench "whitespace string" wsString
        , bench "full string" fullString
        ]


subtractOneUntilZero : Int -> Maybe ( Int, Int )
subtractOneUntilZero i =
    if i /= 0 then
        Just ( i, i - 1 )

    else
        Nothing


ints1To100 : Array Int
ints1To100 =
    Array.fromList (List.range 1 100)



-- Note for benchmarking:
-- Since sets are ordered internally, you can get seriously distorted numbers
-- if the sets happen to intersect and start/end, or are disjoint with no overlap
-- in their ranges.
--
-- Hence the following sample data is carefully chosen such that even disjoint
-- sets are for instance even/odd numbers.
--
-- That said, in my experience specific benchmark results can be highly dependant
-- on exact choice of example data, so if you are getting particularly interesting
-- results, play around with different data distributions.


evenNumberSet : Set Int
evenNumberSet =
    Set.fromList (List.range 50 1000 |> List.filter (\x -> modBy 2 x == 0))


oddNumberSetPlus500 : Set Int
oddNumberSetPlus500 =
    oddNumberSet
        |> Set.insert 500


oddNumberSet : Set Int
oddNumberSet =
    Set.fromList (List.range 1 950 |> List.filter (\x -> modBy 2 x == 1))


lowNumsAndDivisibleBy4Set : Set Int
lowNumsAndDivisibleBy4Set =
    Set.fromList (List.range 1 1000 |> List.filter (\x -> modBy 4 x == 0))
        |> Set.union (Set.fromList (List.range 1 250))


divisibleBy3and5Set : Set Int
divisibleBy3and5Set =
    Set.fromList (List.range 1 1000 |> List.filter (\x -> modBy 3 x == 0))
        |> Set.union (Set.fromList (List.range 1 1000 |> List.filter (\x -> modBy 5 x == 0)))


setExtra : Benchmark
setExtra =
    describe "Set.Extra"
        [ rank "areDisjoint == True"
            (\areDisjoint -> areDisjoint evenNumberSet oddNumberSet)
            [ ( "intersection", Set.Extra.AreDisjoint.intersection )
            , ( "listRecursion", Set.Extra.AreDisjoint.listRecursion )
            , ( "foldr", Set.Extra.AreDisjoint.foldr )
            , ( "foldl", Set.Extra.AreDisjoint.foldl )
            ]
        , rank "areDisjoint == False (and small)"
            (\areDisjoint -> areDisjoint evenNumberSet oddNumberSetPlus500)
            [ ( "intersection", Set.Extra.AreDisjoint.intersection )
            , ( "listRecursion", Set.Extra.AreDisjoint.listRecursion )
            , ( "foldr", Set.Extra.AreDisjoint.foldr )
            , ( "foldl", Set.Extra.AreDisjoint.foldl )
            ]
        , rank "areDisjoint == False (and large)"
            (\areDisjoint -> areDisjoint evenNumberSet lowNumsAndDivisibleBy4Set)
            [ ( "intersection", Set.Extra.AreDisjoint.intersection )
            , ( "listRecursion", Set.Extra.AreDisjoint.listRecursion )
            , ( "foldr", Set.Extra.AreDisjoint.foldr )
            , ( "foldl", Set.Extra.AreDisjoint.foldl )
            ]
        , rank "symmetricDifference"
            (\areDisjoint -> areDisjoint evenNumberSet divisibleBy3and5Set)
            [ ( "naive", Set.Extra.SymmetricDifference.naive )
            , ( "orderExploiting", Set.Extra.SymmetricDifference.orderExploiting )
            ]
        ]
