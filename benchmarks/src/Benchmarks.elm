module Benchmarks exposing (suite)

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
import Bench exposing (Benchmark)
import List.Extra
import List.Extra.DropRight
import List.Extra.GroupsOf
import List.Extra.InsertAt
import List.Extra.Lift
import List.Extra.NotMember
import List.Extra.TakeRight
import List.Extra.Unfoldr
import List.Extra.UniquePairs
import Maybe.Extra.AndMap
import Result.Extra
import Result.Extra.AndMap
import Set exposing (Set)
import Set.Extra.AreDisjoint
import Set.Extra.SymmetricDifference
import String.Extra.IsBlank
import String.Extra.RightOfLeftOf


suite : Benchmark
suite =
    Bench.describe "core-extra"
        [ application
        , array
        , arrayExtra
        , listExtra
        , tupleExtra
        , setExtra
        , stringExtra
        , maybeExtra
        , resultExtra
        ]


application : Benchmark
application =
    Bench.describe "application"
        [ Bench.rank "curry"
            (\sum -> ints1To100 |> sum)
            [ ( "name only", Application.Sum.nameOnlyCurried )
            , ( "partially curried/applied", Application.Sum.partiallyCurried )
            , ( "lambda, piping", Application.Sum.pipe )
            , ( "lambda, fully applied", Application.Sum.lambdaFullyAppliedCurried )
            , ( "lambda nested, fully applied", Application.Sum.lambdaNestedFullyAppliedCurried )
            ]
        , Bench.rank "chain"
            (\negAbs -> ints1To100 |> Array.map negAbs)
            [ ( "declaration argument, |> |>", Application.NegAbs.declarationArgumentPipeline )
            , ( "lambda, |> |>", Application.NegAbs.lambdaPipeline )
            , ( "lambda, |> >>", Application.NegAbs.lambdaPipeComposeR )
            , ( ">>", Application.NegAbs.composeR )
            ]
        ]


array : Benchmark
array =
    Bench.describe "Array"
        [ Bench.rank "Array.fold"
            (\fold -> ints1To100 |> fold (+) 0)
            [ ( "foldl", Array.foldl )
            , ( "foldr", Array.foldr )
            ]
        ]


arrayExtra : Benchmark
arrayExtra =
    Bench.describe "Array.Extra"
        [ Bench.rank "mapToList"
            (\mapToList -> ints1To100 |> mapToList negate)
            [ ( "with foldr", Array.Extra.MapToList.withFoldr )
            , ( "with Array.toIndexedList", Array.Extra.MapToList.withListMap )
            ]
        , Bench.rank "indexedMapToList"
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
        , Bench.rank "reverse"
            (\reverse -> reverse ints1To100)
            [ ( "with cons", Array.Extra.Reverse.withCons )
            , ( "with List.reverse", Array.Extra.Reverse.withListReverse )
            , ( "with push", Array.Extra.Reverse.withPush )
            ]
        , let
            zipped =
                Array.zip ints1To100 ints1To100
          in
          Bench.rank "unzip"
            (\unzip -> zipped |> unzip)
            [ ( "with maps", Array.Extra.Unzip.withMaps )
            , ( "with List.unzip", Array.Extra.Unzip.withListUnzip )
            , ( "with push", Array.Extra.Unzip.wthPush )
            , ( "with cons", Array.Extra.Unzip.wthCons )
            ]
        , Bench.rank "map2"
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
          Bench.rank "filterMap"
            (\filterMap -> maybeInts |> filterMap identity)
            [ ( "with List.filterMap", Array.Extra.FilterMap.withListFilterMap )
            , ( "with push", Array.Extra.FilterMap.withPush )
            , ( "with cons", Array.Extra.FilterMap.withCons )
            ]
        , let
            allTrue =
                Array.repeat 100 True
          in
          Bench.rank "all"
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
          Bench.rank "any"
            (\any -> allFalse |> any identity)
            [ ( "recursive last", Array.Extra.Any.recursiveLast )
            , ( "recursive get", Array.Extra.Any.recursiveGet )
            , ( "with List.any", Array.Extra.Any.withList )
            , ( "with fold", Array.Extra.Any.withFold )
            ]
        , Bench.rank "intersperse"
            (\intersperse -> ints1To100 |> intersperse 0)
            [ ( "with push", Array.Extra.Intersperse.withPush )
            , ( "with cons", Array.Extra.Intersperse.withCons )
            , ( "with List.intersperse", Array.Extra.Intersperse.withListIntersperse )
            ]
        , Bench.rank "member"
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

        longList =
            List.range 1 1000
    in
    Bench.describe "List.Extra"
        ([ Bench.rank "uniquePairs"
            (\uniquePairs -> uniquePairs intList)
            [ ( "original (++)", List.Extra.UniquePairs.originalConcat )
            , ( "tail-recursive", List.Extra.UniquePairs.tailRecursive )
            ]
            -- These return pairs in different order, but this function isn't particularly order sensitive.
            -- Nonetheless, changing the implementation would be a major change.
            |> Bench.skipEqualityCheck
         , Bench.rank "unfoldr"
            (\unfoldr -> unfoldr subtractOneUntilZero 100)
            [ ( "original", List.Extra.Unfoldr.nonTailRecursive )
            , ( "tail-recursive", List.Extra.Unfoldr.tailRecursive )
            ]
         , Bench.rank "lift2"
            (\lift2 -> lift2 (\a b -> a + b) shortList shortList)
            [ ( "original", List.Extra.Lift.liftAndThen2 )
            , ( "foldl", List.Extra.Lift.liftFold2 )
            ]
         , Bench.rank "lift3"
            (\lift3 -> lift3 (\a b c -> a + b + c) shortList shortList shortList)
            [ ( "original", List.Extra.Lift.liftAndThen3 )
            , ( "foldl", List.Extra.Lift.liftFold3 )
            ]
         , Bench.rank "lift4"
            (\lift4 -> lift4 (\a b c d -> a + b + c + d) shortList shortList shortList shortList)
            [ ( "original", List.Extra.Lift.liftAndThen4 )
            , ( "foldl", List.Extra.Lift.liftFold4 )
            ]
         , Bench.rank "notMember 1"
            (\notMember -> notMember 1 intList)
            [ ( "Original", List.Extra.NotMember.notMemberOriginal )
            , ( "Simplified", List.Extra.NotMember.notMemberSimple )
            ]
         , Bench.rank "notMember 99"
            (\notMember -> notMember 99 intList)
            [ ( "Original", List.Extra.NotMember.notMemberOriginal )
            , ( "Simplified", List.Extra.NotMember.notMemberSimple )
            ]
         , Bench.rank "notMember 101"
            (\notMember -> notMember 101 intList)
            [ ( "Original", List.Extra.NotMember.notMemberOriginal )
            , ( "Simplified", List.Extra.NotMember.notMemberSimple )
            ]
         , Bench.rank "dropRight 5 10"
            (\dropRight -> dropRight 5 shortList)
            [ ( "foldr", List.Extra.DropRight.dropRightFoldr )
            , ( "reverse", List.Extra.DropRight.dropRightReverse )
            , ( "length", List.Extra.DropRight.dropRightLength )
            ]
         , Bench.rank "takeRight 5 10"
            (\takeRight -> takeRight 5 shortList)
            [ ( "foldr", List.Extra.TakeRight.takeRightFoldr )
            , ( "reverse", List.Extra.TakeRight.takeRightReverse )
            , ( "length", List.Extra.TakeRight.takeRightLength )
            ]
         , Bench.rank "dropRight 50 100"
            (\dropRight -> dropRight 50 intList)
            [ ( "foldr", List.Extra.DropRight.dropRightFoldr )
            , ( "reverse", List.Extra.DropRight.dropRightReverse )
            , ( "length", List.Extra.DropRight.dropRightLength )
            ]
         , Bench.rank "takeRight 50 100"
            (\takeRight -> takeRight 50 intList)
            [ ( "foldr", List.Extra.TakeRight.takeRightFoldr )
            , ( "reverse", List.Extra.TakeRight.takeRightReverse )
            , ( "length", List.Extra.TakeRight.takeRightLength )
            ]
         , Bench.rank "dropRight 500 1000"
            (\dropRight -> dropRight 500 longList)
            [ ( "foldr", List.Extra.DropRight.dropRightFoldr )
            , ( "reverse", List.Extra.DropRight.dropRightReverse )
            , ( "length", List.Extra.DropRight.dropRightLength )
            ]
         , Bench.rank "takeRight 500 1000"
            (\takeRight -> takeRight 500 longList)
            [ ( "foldr", List.Extra.TakeRight.takeRightFoldr )
            , ( "reverse", List.Extra.TakeRight.takeRightReverse )
            , ( "length", List.Extra.TakeRight.takeRightLength )
            ]
         , Bench.rank "insertAt negative index"
            (\insertAt -> insertAt -3 999 intList)
            [ ( "recursion", List.Extra.InsertAt.insertAtRecursion )
            , ( "takeDrop", List.Extra.InsertAt.insertAtTakeDrop )
            , ( "splitAt", List.Extra.InsertAt.insertAtSplitAt )
            , ( "recursion2", List.Extra.InsertAt.insertAtRecursion2 )
            , ( "recursion3", List.Extra.InsertAt.insertAtRecursion3 )
            ]
         , Bench.rank "insertAt good positive index"
            (\insertAt -> insertAt 50 999 intList)
            [ ( "recursion", List.Extra.InsertAt.insertAtRecursion )
            , ( "takeDrop", List.Extra.InsertAt.insertAtTakeDrop )
            , ( "splitAt", List.Extra.InsertAt.insertAtSplitAt )
            , ( "recursion2", List.Extra.InsertAt.insertAtRecursion2 )
            , ( "recursion3", List.Extra.InsertAt.insertAtRecursion3 )
            ]
         , Bench.rank "insertAt bad positive index"
            (\insertAt -> insertAt 150 999 intList)
            [ ( "recursion", List.Extra.InsertAt.insertAtRecursion )
            , ( "takeDrop", List.Extra.InsertAt.insertAtTakeDrop )
            , ( "splitAt", List.Extra.InsertAt.insertAtSplitAt )
            , ( "recursion2", List.Extra.InsertAt.insertAtRecursion2 )
            , ( "recursion3", List.Extra.InsertAt.insertAtRecursion3 )
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
    [ Bench.rank ("groupsOfWithStep 3 2 [1.." ++ String.fromInt listSize ++ "]")
        (\impl -> impl 3 2 range)
        [ ( "using elm-core's List.tail", List.Extra.GroupsOf.coreTailGroupsOfWithStep )
        , ( "using fully tail-recursive List.tail", List.Extra.GroupsOf.tailRecGroupsOfWithStep )
        ]
    , Bench.rank ("greedyGroupsOfWithStep 3 2 [1.." ++ String.fromInt listSize ++ "]")
        (\impl -> impl 3 2 range)
        [ ( "using elm-core's List.tail", List.Extra.GroupsOf.coreTailGreedyGroupsOfWithStep )
        , ( "using fully tail-recursive List.tail", List.Extra.GroupsOf.tailRecGreedyGroupsOfWithStep )
        ]
    ]


tupleExtra : Benchmark
tupleExtra =
    Bench.describe "Tuple.Extra"
        [ Bench.compare "construction"
            ( "literal", \_ -> ( 1, "a" ) )
            ( "function", \_ -> Tuple.pair 1 "a" )
        ]


stringExtra : Benchmark
stringExtra =
    Bench.describe "String.Extra"
        [ stringExtraIsBlank
        , Bench.describe "String.Extra.{rightOf,leftOf}"
            [ Bench.describe "1 match" (rightLeft 1)
            , Bench.describe "10 matches" (rightLeft 10)
            , Bench.describe "100 matches" (rightLeft 100)
            , Bench.describe "1000 matches" (rightLeft 1000)
            ]
        ]


rightLeft : Int -> List Benchmark
rightLeft matches =
    let
        a =
            List.Extra.initialize matches String.fromInt
                |> String.join "___"
    in
    [ Bench.rank "rightOf"
        (\rightOf -> rightOf "___" a)
        [ ( "regex", String.Extra.RightOfLeftOf.rightOfRegex )
        , ( "String.indexes", String.Extra.RightOfLeftOf.rightOfIndexes )
        ]
    , Bench.rank "leftOf"
        (\leftOf -> leftOf "___" a)
        [ ( "regex", String.Extra.RightOfLeftOf.leftOfRegex )
        , ( "String.indexes", String.Extra.RightOfLeftOf.leftOfIndexes )
        ]
    ]


maybeExtra : Benchmark
maybeExtra =
    Bench.describe "Maybe.Extra"
        [ Bench.rank "andMap - Just × Just"
            (\andMap -> Just negate |> andMap (Just 0))
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        , Bench.rank "andMap - Nothing × Just"
            (\andMap -> Nothing |> andMap (Just 0))
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        , Bench.rank "andMap - Just × Nothing"
            (\andMap -> Just negate |> andMap Nothing)
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        , Bench.rank "andMap - Nothing × Nothing"
            (\andMap -> Nothing |> andMap Nothing)
            [ ( "original", Maybe.Extra.AndMap.andMapOriginal )
            , ( "inlined", Maybe.Extra.AndMap.andMapInlined )
            , ( "simplified", Maybe.Extra.AndMap.andMapSimplified )
            , ( "nested case-of", Maybe.Extra.AndMap.andMapNestedCaseOf )
            , ( "nested case-of ignoring Nothing", Maybe.Extra.AndMap.andMapNestedCaseOfIgnoringNothing )
            ]
        ]


resultExtra : Benchmark
resultExtra =
    let
        integers =
            List.range 0 100

        foldFnAllOk a sum =
            if sum < 0 then
                Err ()

            else
                Ok (a + sum)

        foldFnFirstError a sum =
            if sum <= 0 then
                Err ()

            else
                Ok (a + sum)
    in
    Bench.describe "Result.Extra"
        [ Bench.rank "andMap - Ok × Ok"
            (\andMap -> Ok negate |> andMap (Ok 0))
            [ ( "original", Result.Extra.AndMap.andMapOriginal )
            , ( "inlined", Result.Extra.AndMap.andMapInlined )
            , ( "inlined, nested case-of", Result.Extra.AndMap.andMapInlinedNestedCaseOf )
            ]
        , Bench.rank "andMap - Err × Ok"
            (\andMap -> Err "e" |> andMap (Ok 0))
            [ ( "original", Result.Extra.AndMap.andMapOriginal )
            , ( "inlined", Result.Extra.AndMap.andMapInlined )
            , ( "inlined, nested case-of", Result.Extra.AndMap.andMapInlinedNestedCaseOf )
            ]
        , Bench.rank "andMap - Ok × Err"
            (\andMap -> Ok negate |> andMap (Err "e"))
            [ ( "original", Result.Extra.AndMap.andMapOriginal )
            , ( "inlined", Result.Extra.AndMap.andMapInlined )
            , ( "inlined, nested case-of", Result.Extra.AndMap.andMapInlinedNestedCaseOf )
            ]
        , Bench.rank "andMap - Err × Err"
            (\andMap -> Err "b" |> andMap (Err "e"))
            [ ( "original", Result.Extra.AndMap.andMapOriginal )
            , ( "inlined", Result.Extra.AndMap.andMapInlined )
            , ( "inlined, nested case-of", Result.Extra.AndMap.andMapInlinedNestedCaseOf )
            ]
        , Bench.rank "andMap - Err × Err (2)"
            (\andMap -> Err "l" |> andMap (Err "e"))
            [ ( "original", Result.Extra.AndMap.andMapOriginal )
            , ( "inlined", Result.Extra.AndMap.andMapInlined )
            ]
        , Bench.rank "foldlWhileOk - Err at the first element"
            (\foldlWhileOk -> foldlWhileOk foldFnFirstError 0 integers)
            [ ( "Using List.foldl", \f initial list -> List.foldl (\n acc -> Result.andThen (f n) acc) (Ok initial) list )
            , ( "foldlWhileOk", \f initial list -> Result.Extra.foldlWhileOk f initial list )
            ]
        , Bench.rank "foldlWhileOk - all Ok"
            (\foldlWhileOk -> foldlWhileOk foldFnAllOk 0 integers)
            [ ( "Using List.foldl", \f initial list -> List.foldl (\n acc -> Result.andThen (f n) acc) (Ok initial) list )
            , ( "foldlWhileOk", \f initial list -> Result.Extra.foldlWhileOk f initial list )
            ]
        ]


stringExtraIsBlank : Benchmark
stringExtraIsBlank =
    let
        bench label string =
            Bench.rank label
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
    Bench.describe "isBlank"
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
    Bench.describe "Set.Extra"
        [ Bench.rank "areDisjoint == True"
            (\areDisjoint -> areDisjoint evenNumberSet oddNumberSet)
            [ ( "intersection", Set.Extra.AreDisjoint.intersection )
            , ( "listRecursion", Set.Extra.AreDisjoint.listRecursion )
            , ( "foldr", Set.Extra.AreDisjoint.foldr )
            , ( "foldl", Set.Extra.AreDisjoint.foldl )
            ]
        , Bench.rank "areDisjoint == False (and small)"
            (\areDisjoint -> areDisjoint evenNumberSet oddNumberSetPlus500)
            [ ( "intersection", Set.Extra.AreDisjoint.intersection )
            , ( "listRecursion", Set.Extra.AreDisjoint.listRecursion )
            , ( "foldr", Set.Extra.AreDisjoint.foldr )
            , ( "foldl", Set.Extra.AreDisjoint.foldl )
            ]
        , Bench.rank "areDisjoint == False (and large)"
            (\areDisjoint -> areDisjoint evenNumberSet lowNumsAndDivisibleBy4Set)
            [ ( "intersection", Set.Extra.AreDisjoint.intersection )
            , ( "listRecursion", Set.Extra.AreDisjoint.listRecursion )
            , ( "foldr", Set.Extra.AreDisjoint.foldr )
            , ( "foldl", Set.Extra.AreDisjoint.foldl )
            ]
        , Bench.rank "symmetricDifference"
            (\symDiff -> symDiff evenNumberSet divisibleBy3and5Set)
            [ ( "naive", Set.Extra.SymmetricDifference.naive )
            , ( "orderExploiting", Set.Extra.SymmetricDifference.orderExploiting )
            ]
        ]
