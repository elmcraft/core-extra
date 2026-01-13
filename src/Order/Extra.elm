module Order.Extra exposing
    ( explicit, byField, byFieldWith, byRank, ifStillTiedThen
    , breakTies, breakTiesWith, reverse
    , natural
    , nothingLast, nothingFirst
    , isOrdered, greaterThanBy, lessThanBy
    )

{-| Library for building comparison functions.

This library makes it easy to create comparison functions for arbitary types by composing
smaller comparison functions. For instance, suppose you are defining a data type to represent
a standard deck of cards. You might define it as:

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

With this representation, you could define an ordering for `Card` values compositionally:

    import Order.Extra

    cardOrdering : Card -> Card -> Order
    cardOrdering =
        Ordering.byFieldWith suiteOrdering .suite
            |> Ordering.breakTiesWith
                (Ordering.byFieldWith valueOrdering .value)

    suiteOrdering : Suite -> Suite -> Order
    suiteOrdering =
        Ordering.explicit [ Clubs, Hearts, Diamonds, Spades ]

    valueOrdering : Value -> Value -> Order
    valueOrdering =
        Ordering.explicit
            [ Two
            , Three
            , Four
            , Five
            , Six
            , Seven
            , Eight
            , Nine
            , Ten
            , Jack
            , Queen
            , King
            , Ace
            ]

You can then use this ordering to sort cards, make comparisons, and so on. For instance,
to sort a deck of cards you can use `cardOrdering` directly:

    sortCards : List Card -> List Card
    sortCards =
        List.sortWith cardOrdering


# Construction

@docs explicit, byField, byFieldWith, byRank, ifStillTiedThen


# Composition

@docs breakTies, breakTiesWith, reverse


# Strings

@docs natural


# Maybes

@docs nothingLast, nothingFirst


# Utility

@docs isOrdered, greaterThanBy, lessThanBy

-}

import Regex exposing (Regex)
import String.Extra


{-| Creates an ordering that orders items in the order given in the input list.
Items that are not part of the input list are all considered to be equal to each
other and less than anything in the list.

    type Day
        = Mon
        | Tue
        | Wed
        | Thu
        | Fri
        | Sat
        | Sun

    dayOrdering : Day -> Day -> Order
    dayOrdering =
        Order.Extra.explicit
            [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]

-}
explicit : List a -> a -> a -> Order
explicit elements x y =
    let
        scanForEither items =
            case items of
                z :: zs ->
                    if z == x then
                        scanForY zs

                    else if z == y then
                        scanForX zs

                    else
                        scanForEither zs

                [] ->
                    EQ

        scanForX items =
            case items of
                z :: zs ->
                    if z == x then
                        GT

                    else
                        scanForX zs

                [] ->
                    LT

        scanForY items =
            case items of
                z :: zs ->
                    if z == y then
                        LT

                    else
                        scanForY zs

                [] ->
                    GT
    in
    if x == y then
        EQ

    else
        scanForEither elements


{-| Produces an ordering that orders its elements using the natural ordering of the
field selected by the given function.

    type alias Point = { x : Int, y : Int }

    List.sortWith (Order.Extra.byField .x) [Point 3 5, Point 1 6]
        --> [Point 1 6, Point 3 5]
    List.sortWith (Order.Extra.byField .y) [Point 3 5, Point 1 6]
        --> [Point 3 5, Point 1 6]

-}
byField : (a -> comparable) -> a -> a -> Order
byField x y =
    byFieldWith Basics.compare x y


{-| Produces an ordering that orders its elements using the given ordering on the
field selected by the given function.

    cards : List Card
    cards =
        [ Card King Hearts, Card King Hearts ]

    List.sortWith
        (Order.Extra.byFieldWith valueOrdering .value)
        cards
        == [ Card Two Spades,  Card King Hearts]

    List.sortWith
        (Order.Extra.byFieldWith suiteOrdering .suite)
        cards
        == [ Card King Hearts, Card Two Spades ]

-}
byFieldWith : (b -> b -> Order) -> (a -> b) -> a -> a -> Order
byFieldWith compareField extractField x y =
    compareField (extractField x) (extractField y)


{-| Produces an ordering that refines the second input ordering by using the first
a -> a -> Orders a tie breaker. (Note that the second argument is the primary sort, and
the first argument is a tie breaker. This argument ordering is intended to support
function chaining with `|>`.)

    type alias Point =
        { x : Int, y : Int }

    pointOrdering : Point -> Point -> Order
    pointOrdering =
        Order.Extra.byField .x
            |> Order.Extra.breakTiesWith (Order.Extra.byField .y)

-}
breakTiesWith : (a -> a -> Order) -> (a -> a -> Order) -> a -> a -> Order
breakTiesWith tiebreaker mainOrdering x y =
    case mainOrdering x y of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            tiebreaker x y


{-| Create an ordering function that can be used to sort
lists by multiple dimensions, by flattening multiple ordering functions into one.

This is equivalent to `ORDER BY` in SQL. The ordering function will order
its inputs based on the order that they appear in the `List (a -> a -> Order)` argument.

    type alias Pen =
        { model : String
        , tipWidthInMillimeters : Float
        }

    pens : List Pen
    pens =
        [ Pen "Pilot Hi-Tec-C Gel" 0.4
        , Pen "Morning Glory Pro Mach" 0.38
        , Pen "Pilot Hi-Tec-C Coleto" 0.5
        ]

    order : Pen -> Pen -> Order
    order =
        breakTies [ byField .tipWidthInMillimeters, byField .model ]

    List.sortWith order pens
    --> [ Pen "Morning Glory Pro Mach" 0.38
    --> , Pen "Pilot Hi-Tec-C Gel" 0.4
    --> , Pen "Pilot Hi-Tec-C Coleto" 0.5
    --> ]

If our `Pen` type alias above was represented a row in a database table, our `order` function as defined above would be equivalent
to this SQL clause:

    ORDER BY tipWidthInMillimeters, model

-}
breakTies : List (a -> a -> Order) -> (a -> a -> Order)
breakTies comparators a b =
    case comparators of
        [] ->
            EQ

        comparator :: rest ->
            case comparator a b of
                EQ ->
                    breakTies rest a b

                other ->
                    other


{-| Produces an ordering defined by an explicit ranking function combined with a
secondary ordering function to compare elements within the same rank. The rule is
that all items are sorted first by rank, and then using the given within-rank
ordering for items of the same rank.

This function is intended for use with types that have multiple cases where
constructors for some or all of the cases take arguments. (Otherwise use `Ordering.explicit`
instead which has a simpler interface.) For instance, to make an ordering for
a type such as:

    type JokerCard
        = NormalCard Value Suite
        | Joker

you could use `byRank` to sort all the normal cards before the jokers like so:

    jokerCardOrdering : JokerCard -> JokerCard -> Order
    jokerCardOrdering =
        Order.Extra.byRank
            (\card ->
                case card of
                    NormalCard _ _ ->
                        1

                    Joker ->
                        2
            )
            (\x y ->
                case ( x, y ) of
                    ( NormalCard v1 s1, NormalCard v2 s2 ) ->
                        suiteOrdering s1 s2
                            |> Order.Extra.ifStillTiedThen
                                (valueOrdering v1 v2)

                    _ ->
                        EQ
            )

More generally, the expected pattern is that for each case in your type, you assign
that case to a unique rank with the ranking function. Then for your within-rank
ordering, you have a case statement that enumerates all the "tie" states and
specifies how to break ties, and then uses a catch-all case that returns
`Ordering.noConflicts` to specify that all remaining cases cannot give rise to
the need to do any subcomparisons. (This can be either because the values being
compared have no internal structure and so are always equal, or because they are
constructors with different ranks and so will never be compared by this function.)

-}
byRank : (a -> Int) -> (a -> a -> Order) -> a -> a -> Order
byRank rank withinRankOrdering =
    byField rank |> breakTiesWith withinRankOrdering


{-| Returns the main order unless it is `EQ`, in which case returns the tiebreaker.

This function does for `Order`s what `breakTiesWith` does for `Ordering`s. It is
useful in cases where you want to perform a cascading comparison of multiple pairs
of values that are not wrapped in a container value, as happens when examining the
individual fields of a constructor.

-}
ifStillTiedThen : Order -> Order -> Order
ifStillTiedThen tiebreaker mainOrder =
    case mainOrder of
        EQ ->
            tiebreaker

        _ ->
            mainOrder


{-| Returns an ordering that reverses the input ordering.

    List.sortWith
        (Order.Extra.reverse compare)
        [ 1, 2, 3, 4, 5 ]
        --> [ 5, 4, 3, 2, 1 ]

-}
reverse : (a -> a -> Order) -> a -> a -> Order
reverse ordering x y =
    case ordering x y of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| Determines if the given list is ordered according to the given ordering.

    Order.Extra.isOrdered compare [ 1, 2, 3 ]
        --> True

    Order.Extra.isOrdered compare [ 2, 1, 3 ]
        --> False

    Order.Extra.isOrdered compare []
        --> True

    Order.Extra.isOrdered
        (Order.Extra.reverse compare)
        [ 1, 2, 3 ]
        --> False

-}
isOrdered : (a -> a -> Order) -> List a -> Bool
isOrdered ordering items =
    case items of
        x :: ((y :: _) as rest) ->
            case ordering x y of
                LT ->
                    isOrdered ordering rest

                EQ ->
                    isOrdered ordering rest

                GT ->
                    False

        -- lists of one or zero elements are always ordered
        _ ->
            True


{-| Determines if one value is less than another according to the given ordering.

    lessThanBy
        xThenYOrdering
        { x = 7, y = 8 }
        { x = 10, y = 2 }
        == True

    lessThanBy
        yThenXOrdering
        { x = 7, y = 8 }
        { x = 10, y = 2 }
        == False

-}
lessThanBy : (a -> a -> Order) -> a -> a -> Bool
lessThanBy ordering x y =
    case ordering x y of
        LT ->
            True

        _ ->
            False


{-| Determines if one value is greater than another according to the given ordering.

    greaterThanBy
        xThenYOrdering
        { x = 7, y = 8 }
        { x = 10, y = 2 }
        == False

    greaterThanBy
        yThenXOrdering
        { x = 7, y = 8 }
        { x = 10, y = 2 }
        == True

-}
greaterThanBy : (a -> a -> Order) -> a -> a -> Bool
greaterThanBy ordering x y =
    case ordering x y of
        GT ->
            True

        _ ->
            False



--- Maybe


{-| Returns an ordering that treats `Nothing` as greater than `Just a`.

    [ Just 1, Nothing, Just 2 ]
        |> List.sortWith (Order.Extra.nothingLast compare)
        --> [ Just 1, Just 2, Nothing ]

-}
nothingLast : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
nothingLast ordering x y =
    case ( x, y ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, Just _ ) ->
            GT

        ( Just _, Nothing ) ->
            LT

        ( Just a, Just b ) ->
            ordering a b


{-| Returns an ordering that treats `Nothing` as less than `Just a`.

    [ Just 1, Nothing, Just 2 ]
        |> List.sortWith (Order.Extra.nothingFirst compare)
        --> [ Nothing, Just 1, Just 2 ]

-}
nothingFirst : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
nothingFirst ordering x y =
    case ( x, y ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, Just _ ) ->
            LT

        ( Just _, Nothing ) ->
            GT

        ( Just a, Just b ) ->
            ordering a b



--- String


{-| Compare two strings naturally.

    List.sortWith Order.Extra.natural ["a10", "a2"]
    --> ["a2", "a10"]

Without full I18n support, this is probably the best way to sort
user provided strings in a way that is intuitive to humans.

-}
natural : String -> String -> Order
natural x y =
    compareChunkLists (toChunks x) (toChunks y)


type Chunk
    = StringChunk String
    | IntChunk Int


compareChunks : Chunk -> Chunk -> Order
compareChunks chunk1 chunk2 =
    case ( chunk1, chunk2 ) of
        ( StringChunk str1, StringChunk str2 ) ->
            Basics.compare (toComparableString str1) (toComparableString str2)

        ( StringChunk _, IntChunk _ ) ->
            GT

        ( IntChunk _, StringChunk _ ) ->
            LT

        ( IntChunk int1, IntChunk int2 ) ->
            Basics.compare int1 int2


compareChunkLists : List Chunk -> List Chunk -> Order
compareChunkLists chunkList1 chunkList2 =
    case ( chunkList1, chunkList2 ) of
        ( [], [] ) ->
            EQ

        ( [], _ :: _ ) ->
            LT

        ( _ :: _, [] ) ->
            GT

        ( chunk1 :: chunks1, chunk2 :: chunks2 ) ->
            case compareChunks chunk1 chunk2 of
                EQ ->
                    compareChunkLists chunks1 chunks2

                ord ->
                    ord


chunkRegex : Regex
chunkRegex =
    Regex.fromString "[0-9]+|[^0-9]+"
        |> Maybe.withDefault Regex.never


toComparableString : String -> String
toComparableString str =
    String.toLower (String.Extra.removeDiacritics str)


toChunks : String -> List Chunk
toChunks str =
    str
        |> Regex.find chunkRegex
        |> List.map (\{ match } -> toChunk match)


toChunk : String -> Chunk
toChunk str =
    String.toInt str
        |> Maybe.andThen intToChunk
        |> Maybe.withDefault (StringChunk str)


intToChunk : Int -> Maybe Chunk
intToChunk int =
    if isNaN (toFloat int) then
        Nothing

    else
        Just (IntChunk int)
