module OrderTests exposing (..)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Order.Extra exposing (Ordering)
import Test exposing (..)


type alias Card =
    { value : Value, suite : Suite }


type Suite
    = Clubs
    | Hearts
    | Diamonds
    | Spades


type Value
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type JokerCard
    = NormalCard Value Suite
    | Joker


suiteOrdering : Ordering Suite
suiteOrdering =
    Order.Extra.explicit [ Clubs, Hearts, Diamonds, Spades ]


valueOrdering : Ordering Value
valueOrdering =
    Order.Extra.explicit [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


cardOrdering : Ordering Card
cardOrdering =
    Order.Extra.byFieldWith suiteOrdering .suite
        |> Order.Extra.breakTiesWith (Order.Extra.byFieldWith valueOrdering .value)


sortCards : List Card -> List Card
sortCards =
    List.sortWith cardOrdering


oneOf : List a -> Fuzzer a
oneOf items =
    Fuzz.frequency (List.map (\x -> ( 1, Fuzz.constant x )) items)


suite : Fuzzer Suite
suite =
    oneOf [ Clubs, Hearts, Diamonds, Spades ]


value : Fuzzer Value
value =
    oneOf [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


card : Fuzzer Card
card =
    Fuzz.map2 Card value suite


deck : Fuzzer (List Card)
deck =
    Fuzz.list card


type alias Point =
    { x : Int, y : Int }


point : Fuzzer Point
point =
    Fuzz.map2 Point Fuzz.int Fuzz.int


pointOrdering : Ordering Point
pointOrdering =
    Order.Extra.byField .x
        |> Order.Extra.breakTiesWith (Order.Extra.byField .y)


expectOrdered : Ordering a -> List a -> Expect.Expectation
expectOrdered ordering list =
    Order.Extra.isOrdered ordering list
        |> Expect.true "Expected ordered list"


expectNotOrdered : Ordering a -> List a -> Expect.Expectation
expectNotOrdered ordering list =
    Order.Extra.isOrdered ordering list
        |> Expect.false "Expected list to be out of order"


categorize : (a -> comparable) -> List a -> List (List a)
categorize categorizer items =
    List.foldl
        (\item dict ->
            let
                category =
                    categorizer item
            in
            Dict.update category
                (\v ->
                    case v of
                        Just oldElements ->
                            Just (oldElements ++ [ item ])

                        Nothing ->
                            Just [ item ]
                )
                dict
        )
        Dict.empty
        items
        |> Dict.values


all : Test
all =
    describe "Ordering"
        [ describe "isOrdered"
            [ test "ascending list" <|
                \_ -> expectOrdered Order.Extra.natural [ 1, 2, 3 ]
            , test "out-of-order list" <|
                \_ ->
                    expectNotOrdered Order.Extra.natural [ 2, 1, 3 ]
            , test "empty list" <|
                \_ ->
                    expectOrdered Order.Extra.natural []
            , test "singleton list" <|
                \_ ->
                    expectOrdered Order.Extra.natural [ 1 ]
            ]
        , describe "lessThanBy / greaterThanBy"
            (let
                xThenYOrdering =
                    Order.Extra.byField .x |> Order.Extra.breakTiesWith (Order.Extra.byField .y)

                yThenXOrdering =
                    Order.Extra.byField .y |> Order.Extra.breakTiesWith (Order.Extra.byField .x)

                point1 =
                    { x = 7, y = 8 }

                point2 =
                    { x = 10, y = 2 }
             in
             [ test "lessThanBy true" <|
                \_ ->
                    Order.Extra.lessThanBy xThenYOrdering point1 point2 |> Expect.true "expected ordered elements"
             , test "greaterThanBy false" <|
                \_ ->
                    Order.Extra.greaterThanBy xThenYOrdering point1 point2 |> Expect.false "expected out of order elements"
             , test "lessThanBy false" <|
                \_ ->
                    Order.Extra.lessThanBy yThenXOrdering point1 point2 |> Expect.false "expected out-of-order elements"
             , test "greaterThanBy true" <|
                \_ ->
                    Order.Extra.greaterThanBy yThenXOrdering point1 point2 |> Expect.true "expected ordered elements"
             , fuzz (Fuzz.tuple ( point, point )) "greaterThanBy and lessThanBy behave as ordering functions" <|
                \( p1, p2 ) ->
                    case ( Order.Extra.lessThanBy pointOrdering p1 p2, Order.Extra.greaterThanBy pointOrdering p1 p2 ) of
                        ( True, True ) ->
                            Expect.fail "Point 1 is both greater than and less than point 2"

                        _ ->
                            Expect.pass
             ]
            )
        , describe "explicit"
            [ test "ordered" <|
                \_ -> expectOrdered suiteOrdering [ Clubs, Hearts, Diamonds, Spades ]
            , test "equal" <|
                \_ ->
                    suiteOrdering Hearts Hearts
                        |> Expect.equal EQ
            , test "less than" <|
                \_ ->
                    suiteOrdering Hearts Spades
                        |> Expect.equal LT
            , test "greater than" <|
                \_ ->
                    suiteOrdering Diamonds Clubs
                        |> Expect.equal GT
            , let
                orderedValues =
                    [ Three, Four, Five, Six, Seven, Eight, Nine, Ten ]

                excludedValues =
                    [ Two, Jack, Queen, King, Ace ]

                partialValueOrdering =
                    Order.Extra.explicit orderedValues

                includedValue =
                    oneOf orderedValues

                excludedValue =
                    oneOf excludedValues
              in
              fuzz2 includedValue
                excludedValue
                "explicit ordering sorts unlisted items as less than"
              <|
                \included excluded ->
                    Order.Extra.lessThanBy partialValueOrdering excluded included
                        |> Expect.true "Expected excluded value from partial ordering to be less than included"
            ]
        , describe "byField"
            [ fuzz (Fuzz.list point) "Ordering list of points by field produces ascending ordered values" <|
                \points ->
                    let
                        xCoordsOfOrderedPoints =
                            List.sortWith (Order.Extra.byField .x) points |> List.map .x

                        orderedXCoords =
                            List.sort (List.map .x points)
                    in
                    Expect.equal orderedXCoords xCoordsOfOrderedPoints
            ]
        , describe "reverse"
            [ fuzz (Fuzz.list point) "Reversing the result of reverse sort is the same as the forward sort" <|
                \points ->
                    let
                        sortedPoints =
                            List.sortWith pointOrdering points

                        reverseSortedPoints =
                            List.sortWith (Order.Extra.reverse pointOrdering) points
                    in
                    Expect.equal (List.reverse sortedPoints) reverseSortedPoints
            ]
        , describe "breakTiesWith"
            [ fuzzWith { runs = 1000 }
                (Fuzz.list (Fuzz.tuple3 ( Fuzz.int, Fuzz.int, Fuzz.int )))
                "Breaking ties three ways works"
              <|
                \triples ->
                    let
                        fst ( x, _, _ ) =
                            x

                        snd ( _, y, _ ) =
                            y

                        thd ( _, _, z ) =
                            z

                        sorted =
                            List.sortWith
                                (Order.Extra.byField fst
                                    |> Order.Extra.breakTiesWith (Order.Extra.byField snd)
                                    |> Order.Extra.breakTiesWith (Order.Extra.byField thd)
                                )
                                triples

                        firstsSorted =
                            Order.Extra.isOrdered Order.Extra.natural (List.map fst sorted)

                        categorizedByFirst =
                            categorize fst sorted

                        secondsSorted =
                            List.map (\triples_ -> Order.Extra.isOrdered Order.Extra.natural (List.map snd triples_))
                                categorizedByFirst

                        categorizedByFirstAndSecond =
                            categorize (\( x, y, _ ) -> ( x, y )) sorted

                        thirdsSorted =
                            List.map (\triples_ -> Order.Extra.isOrdered Order.Extra.natural (List.map thd triples_))
                                categorizedByFirstAndSecond
                    in
                    Expect.true "Something wasn't sorted"
                        (firstsSorted
                            && List.all (\x -> x) secondsSorted
                            && List.all (\x -> x) thirdsSorted
                        )
            ]
        , describe "byRank" <|
            let
                jokerCardOrdering =
                    Order.Extra.byRank
                        (\card_ ->
                            case card_ of
                                NormalCard _ _ ->
                                    1

                                Joker ->
                                    2
                        )
                        (\x y ->
                            case ( x, y ) of
                                ( NormalCard v1 s1, NormalCard v2 s2 ) ->
                                    suiteOrdering s1 s2
                                        |> Order.Extra.ifStillTiedThen (valueOrdering v1 v2)

                                _ ->
                                    Order.Extra.noConflicts
                        )
            in
            [ test "Orders jokers first" <|
                \_ ->
                    List.sortWith jokerCardOrdering [ NormalCard Three Spades, Joker, NormalCard Five Clubs, Joker ]
                        |> Expect.equal [ NormalCard Five Clubs, NormalCard Three Spades, Joker, Joker ]
            ]
        , describe "Overall sorting tests with cards"
            [ test "Cards are ordered" <|
                \_ ->
                    sortCards [ Card Two Spades, Card King Diamonds ]
                        |> Expect.equal [ Card King Diamonds, Card Two Spades ]
            , test "Cards in same suite are ordered" <|
                \_ ->
                    sortCards [ Card Ten Spades, Card Four Hearts, Card Three Spades ]
                        |> Expect.equal [ Card Four Hearts, Card Three Spades, Card Ten Spades ]
            ]
        ]
