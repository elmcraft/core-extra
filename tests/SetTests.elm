module SetTests exposing (all)

import Basics.Extra exposing (flip)
import Expect
import Fuzz exposing (Fuzzer)
import Set exposing (Set)
import Set.Extra
import Test exposing (Test, describe, fuzz, fuzz2, test)


all : Test
all =
    describe "Set.Extra"
        [ describe "#concatMap"
            [ fuzz (Fuzz.list Fuzz.int) "Same as concatMap and from/toList" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.concatMap doubleSet
                        |> Expect.equal
                            (xs
                                |> List.concatMap doubleList
                                |> Set.fromList
                            )
            , fuzz Fuzz.int "left identity" <|
                \x ->
                    Set.singleton x
                        |> Set.Extra.concatMap doubleSet
                        |> Expect.equal (doubleSet x)
            , fuzz (Fuzz.list Fuzz.int) "right identity" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.concatMap Set.singleton
                        |> Expect.equal (Set.fromList xs)
            , fuzz (Fuzz.list Fuzz.int) "associativity" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.concatMap doubleSet
                        |> Set.Extra.concatMap tripleSet
                        |> Expect.equal
                            (Set.fromList xs
                                |> Set.Extra.concatMap
                                    (\x ->
                                        doubleSet x
                                            |> Set.Extra.concatMap tripleSet
                                    )
                            )
            ]
        , describe "#isSubsetOf"
            [ fuzz2 (Fuzz.list Fuzz.int) (Fuzz.list Fuzz.int) "Same as List.Extra.isInfixOf" <|
                \xs ys ->
                    Set.fromList xs
                        |> Set.Extra.isSubsetOf (Set.fromList ys)
                        |> Expect.equal
                            (List.all (flip List.member ys) xs)
            , test "checks if a set is a subset of another set" <|
                \() ->
                    Set.fromList [ 2, 4, 6 ]
                        |> Set.Extra.isSubsetOf (Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8 ])
                        |> Expect.equal True
                        |> Expect.onFail "Expected the Set to be a subset"
            , test "checks if a set isn't a subset of another set" <|
                \() ->
                    Set.fromList [ 2, 4, 10 ]
                        |> Set.Extra.isSubsetOf (Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8 ])
                        |> Expect.equal False
                        |> Expect.onFail "Expected the Set to not be a subset"
            ]
        , describe "#toggle"
            [ fuzz2 (Fuzz.list Fuzz.int) Fuzz.int "Removes an existing element" <|
                \xs x ->
                    let
                        setWithoutX =
                            Set.fromList xs
                                |> Set.remove x

                        setWithX =
                            Set.insert x setWithoutX
                    in
                    Set.Extra.toggle x setWithoutX
                        |> Expect.equalSets setWithX
            , fuzz2 (Fuzz.list Fuzz.int) Fuzz.int "Adds an new element" <|
                \xs x ->
                    let
                        setWithoutX =
                            Set.fromList xs
                                |> Set.remove x

                        setWithX =
                            Set.insert x setWithoutX
                    in
                    Set.Extra.toggle x setWithX
                        |> Expect.equalSets setWithoutX
            ]
        , describe "#filterMap"
            [ test "Applies a function that may succeed to all values in the list, but only keep the successes." <|
                \() ->
                    Set.fromList [ "1", "2", "3", "hello", "4", "world" ]
                        |> Set.Extra.filterMap String.toFloat
                        |> Expect.equal (Set.fromList [ 1, 2, 3, 4 ])
            , fuzz (Fuzz.list Fuzz.string) "should work like (List.filterMap >> Set.fromList)" <|
                \xs ->
                    Set.fromList xs
                        |> Set.Extra.filterMap String.toFloat
                        |> Expect.equal
                            (List.filterMap String.toFloat xs
                                |> Set.fromList
                            )
            ]
        , describe "#areDisjoint"
            [ fuzz2 (fuzzSet Fuzz.int) (fuzzSet Fuzz.int) "disjoint is equivalent to empty intersection" <|
                \a b ->
                    Set.intersect a b
                        |> Set.isEmpty
                        |> Expect.equal (Set.Extra.areDisjoint a b)
            ]
        , describe "#symmetricDifference"
            [ fuzz2 (fuzzSet Fuzz.int) (fuzzSet Fuzz.int) "equivalent to diff (union a b) (intersect a b)" <|
                \a b ->
                    Set.diff (Set.union a b) (Set.intersect a b)
                        |> Expect.equal (Set.Extra.symmetricDifference a b)
            ]
        ]


fuzzSet : Fuzzer comparable -> Fuzzer (Set comparable)
fuzzSet fuzz =
    Fuzz.map Set.fromList (Fuzz.list fuzz)


doubleList : Int -> List Int
doubleList a =
    [ a, 2 * a ]


doubleSet : Int -> Set Int
doubleSet a =
    Set.fromList [ a, 2 * a ]


tripleSet : Int -> Set Int
tripleSet a =
    Set.fromList [ a, 3 * a ]
