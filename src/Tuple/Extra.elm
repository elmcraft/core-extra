module Tuple.Extra exposing
    ( Tuple
    , pairWith, from
    , apply, flip, join, joinBy, sum, product, sort, sortBy, sortWith
    , map
    )

{-| Convenience functions for 2-Tuples (also known as pairs).

@docs Tuple


# Paring

@docs pairWith, from


# Manipulating

@docs apply, flip, join, joinBy, sum, product, sort, sortBy, sortWith


# Mapping

@docs map

-}

-- Types -----------------------------------------------------------------------


{-| By type aliasing tuples into a "normal" type, we remove the (small) effort
required in reading types and signatures that have tuples in. I've found this
is most beneficial when a tuple is nested inside another type. Visually, the
Tuple type is now no different to List, Maybe, or Result.

For example, this:

    List (Maybe ( String, Int ))

becomes:

    List (Maybe (Tuple String Int))

-}
type alias Tuple a b =
    ( a, b )



-- Pairing ---------------------------------------------------------------------


{-| In certain situations, this proves more "pipe friendly" than the standard
`Tuple.pair`. Fits nicely in your `update` function.

    { model | count = model.count + 1 }
        |> Tuple.Extra.pairWith Cmd.none

-}
pairWith : b -> a -> ( a, b )
pairWith b a =
    ( a, b )


{-| Occasionally you might want to create a Tuple from a single value. This does
just that.

    Tuple.Extra.from 1 --> ( 1, 1 )

-}
from : a -> ( a, a )
from a =
    ( a, a )



-- Manipulating ----------------------------------------------------------------


{-| Given a function that takes two arguments, apply that function to the two
values contained in a tuple.

    Tuple.Extra.apply (+) ( 1, 2 )
        --> 3

-}
apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b


{-| Flip the two values contained in a tuple.
-}
flip : ( a, b ) -> ( b, a )
flip ( a, b ) =
    ( b, a )


{-| Similar to String.join but for tuples instead of lists. Given some separator
string, join together two strings in a tuple.

    Tuple.Extra.join " " ( "Hello", "world" )
        --> "Hello world"

-}
join : appendable -> ( appendable, appendable ) -> appendable
join =
    joinBy identity identity


{-| Works just like join, but first converts the values of the tuple to strings.
These means the function works with any type of tuple.

    Tuple.Extra.joinBy String.fromInt suitToString " of " ( 7, Club )
        == "Seven of Clubs"

-}
joinBy : (a -> appendable) -> (b -> appendable) -> appendable -> ( a, b ) -> appendable
joinBy f g s ( a, b ) =
    f a ++ s ++ g b


{-| Similar to List.sum but for tuples instead of lists. Adds together two
numbers contained in a tuple.

    Tuple.Extra.sum ( 1, 2 )
        --> 3

-}
sum : ( number, number ) -> number
sum t =
    apply (+) t


{-| Similar to List.sum but for tuples instead of lists. Multiplies together two
numbers contained in a tuple

    Tuple.Extra.product ( 1, 2 )
        --> 2

-}
product : ( number, number ) -> number
product t =
    apply (*) t


{-| Similar to List.sort but for tuples instead of lists. Sort values contained
in a tuple from lowest to highest

    Tuple.Extra.sort ( 2, 1 )
        --> ( 1, 2 )

-}
sort : ( comparable, comparable ) -> ( comparable, comparable )
sort ( a, b ) =
    if a <= b then
        ( a, b )

    else
        ( b, a )


{-| Similar to List.sortBy but for tuples instead of lists. Sort values
contained in a tuple by first converting both values to a `comparable`. The
values are sorted lowest to highest

    Tuple.Extra.sortBy String.length ( "mouse", "cat" )
        --> ( "cat", "mouse" )

-}
sortBy : (a -> comparable) -> ( a, a ) -> ( a, a )
sortBy toComparable ( a, b ) =
    if toComparable a <= toComparable b then
        ( a, b )

    else
        ( b, a )


{-| Similar to List.sortWith but for tuples instead of lists. Instead of
converting values contained in a tuple to `comparable`s, instead supply a
function that will produce an `Order` directly.

    Tuple.Extra.sortWith Basics.compare ( 2, 1 )
        --> ( 1, 2 )

-}
sortWith : (a -> a -> Order) -> ( a, a ) -> ( a, a )
sortWith toOrder ( a, b ) =
    case toOrder a b of
        LT ->
            ( a, b )

        EQ ->
            ( a, b )

        GT ->
            ( b, a )



-- Mapping ---------------------------------------------------------------------


{-| Apply a function to both values contained in a tuple. This might also be
known as `mapBothWith` or `bimap`.

    Tuple.Extra.map negate ( -3, 10 )
        --> ( 3, -10 )

-}
map : (a -> b) -> ( a, a ) -> ( b, b )
map f ( a, b ) =
    ( f a, f b )
