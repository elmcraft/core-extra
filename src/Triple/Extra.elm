module Triple.Extra exposing
    ( Triple
    , triple, from
    , first, second, third
    , apply, sortWith
    , map, mapFirst, mapSecond, mapThird
    , toList
    )

{-| Convenience functions for 3-Tuples (also known as Triple).

Please remember the following from the Tuple documentation:

> For more complex data, it is best to switch to records. So instead of representing a 3D point as `(3,4,5)` and not having any helper functions, represent it as `{ x = 3, y = 4, z = 5 }` and use all the built-in record syntax!

This is good advice! However triples do have their usecases for anonymous groupings and as such having some helper functions can be quite helpful.

@docs Triple


# Create

@docs triple, from


# Access

@docs first, second, third


# Manipulating

@docs apply, sortWith


# Mapping

@docs map, mapFirst, mapSecond, mapThird


# Lists

@docs toList

-}

-- Types -----------------------------------------------------------------------


{-| By type aliasing Triples into a "normal" type, we remove the (small) effort
required in reading types and signatures that have Triples in. This
is most beneficial when a Triple is nested inside another type. Visually, the
Triple type is now no different to List, Maybe, or Result.

For example, this:

    List (Maybe ( String, Int, Char ))

becomes:

    List (Maybe (Triple String Int Char))

-}
type alias Triple a b c =
    ( a, b, c )


{-| Create a triple.
-}
triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


{-| Occasionally you might want to create a Triple from a single value. This does
just that.

    Triple.Extra.from 1 --> ( 1, 1, 1 )

-}
from : a -> ( a, a, a )
from a =
    ( a, a, a )



-- Access ----------------------------------------------------------------------


{-| Extract the first value from a triple.
-}
first : ( a, b, c ) -> a
first ( a, _, _ ) =
    a


{-| Extract the second value from a triple.
-}
second : ( a, b, c ) -> b
second ( _, b, _ ) =
    b


{-| Extract the third value from a triple.
-}
third : ( a, b, c ) -> c
third ( _, _, c ) =
    c



-- Manipulating ----------------------------------------------------------------


{-| Given a function that takes three arguments, apply that function to the three
values contained in a Triple.

    ( 1, 2, Array.fromList [0,1,2,3,4] )
        |> Triple.Extra.apply Array.slice
        --> Array.fromList [1,2,3]

-}
apply : (a -> b -> c -> d) -> ( a, b, c ) -> d
apply f ( a, b, c ) =
    f a b c


{-| Similar to List.sortWith but for Triples instead of lists. Instead of
converting values contained in a Triple to `comparable`s, instead supply a
function that will produce an `Order` directly.

    Triple.Extra.sortWith Basics.compare ( 2, 1, 3 )
        --> ( 1, 2, 3 )

-}
sortWith : (a -> a -> Order) -> ( a, a, a ) -> ( a, a, a )
sortWith toOrder ( a, b, c ) =
    case ( toOrder a b, toOrder b c ) of
        ( LT, LT ) ->
            ( a, b, c )

        ( GT, GT ) ->
            ( c, b, a )

        ( EQ, EQ ) ->
            ( a, b, c )

        ( LT, EQ ) ->
            ( a, b, c )

        ( EQ, LT ) ->
            ( a, b, c )

        ( EQ, GT ) ->
            ( c, a, b )

        ( GT, EQ ) ->
            ( c, b, a )

        ( GT, LT ) ->
            case toOrder a c of
                GT ->
                    ( b, c, a )

                _ ->
                    ( b, a, c )

        ( LT, GT ) ->
            case toOrder a c of
                GT ->
                    ( c, a, b )

                _ ->
                    ( a, c, b )



-- Mapping ---------------------------------------------------------------------


{-| Transform the first value in a triple.

    Triple.Extra.mapFirst negate ( -3, 10, '9' )
        --> ( 3, 10, '9' )

-}
mapFirst : (a -> x) -> ( a, b, c ) -> ( x, b, c )
mapFirst f ( a, b, c ) =
    ( f a, b, c )


{-| Transform the second value in a triple.

    Triple.Extra.mapSecond negate ( -3, 10, '9' )
        --> ( -3, 010, '9' )

-}
mapSecond : (b -> x) -> ( a, b, c ) -> ( a, x, c )
mapSecond f ( a, b, c ) =
    ( a, f b, c )


{-| Transform the third value in a triple.

    Triple.Extra.mapThird String.toUpper ( -3, 10, "a" )
        --> ( -3, 10, "A" )

-}
mapThird : (c -> x) -> ( a, b, c ) -> ( a, b, x )
mapThird f ( a, b, c ) =
    ( a, b, f c )


{-| Apply a function to all values contained in a Triple.

    Triple.Extra.map negate ( -3, 10, -7 )
        --> ( 3, -10, 7 )

-}
map : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map f ( a, b, c ) =
    ( f a, f b, f c )



-- List conversion --------------------------------------------------------------


{-| Turns a triple into a list of three elements.

    Triple.Extra.toList ( 1, 2, 3 )
        --> [ 1, 2, 3 ]

-}
toList : ( a, a, a ) -> List a
toList ( a, b, c ) =
    [ a, b, c ]
