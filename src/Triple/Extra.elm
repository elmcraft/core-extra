module Triple.Extra exposing
    ( Triple
    , triple, from
    , first, second, third
    , apply, join, joinBy, sum, product, sort, sortBy, sortWith
    , map, mapFirst, mapSecond, mapThird
    , toList, fromListHead
    , getAA, getAB, getAC, getBA, getBB, getBC, getCA, getCB, getCC
    , getAAA, getAAB, getAAC, getABA, getABB, getABC, getACA, getACB, getACC, getBAA, getBAB, getBAC, getBBA, getBBB, getBBC, getBCA, getBCB, getBCC, getCAA, getCAB, getCAC, getCBA, getCBB, getCBC, getCCA, getCCB, getCCC
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

@docs apply, join, joinBy, sum, product, sort, sortBy, sortWith


# Mapping

@docs map, mapFirst, mapSecond, mapThird


# Lists

@docs toList, fromListHead


# Swizzling

Swizzling refers to getting some sort of subselection of fields into a new Tuple. Don't worry, you don't need to check the documentation, we provide all combinations of 2 and 3 fields.


## Swizzling into Pairs

@docs getAA, getAB, getAC, getBA, getBB, getBC, getCA, getCB, getCC


## Sizzling into Triples

@docs getAAA, getAAB, getAAC, getABA, getABB, getABC, getACA, getACB, getACC, getBAA, getBAB, getBAC, getBBA, getBBB, getBBC, getBCA, getBCB, getBCC, getCAA, getCAB, getCAC, getCBA, getCBB, getCBC, getCCA, getCCB, getCCC

-}

import Order.Extra



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


{-| Similar to String.join but for Triples instead of lists. Given some separator
string, join together three strings in a Triple.

    Triple.Extra.join " " ( "Hello", "world", "!" )
        --> "Hello world!"

-}
join : appendable -> ( appendable, appendable, appendable ) -> appendable
join =
    joinBy identity identity identity


{-| Works just like join, but first converts the values of the Triple to strings.
These means the function works with any type of Triple.
-}
joinBy : (a -> appendable) -> (b -> appendable) -> (c -> appendable) -> appendable -> ( a, b, c ) -> appendable
joinBy f g h s ( a, b, c ) =
    f a ++ s ++ g b ++ s ++ h c


{-| Similar to List.sum but for Triples instead of lists. Adds together three
numbers contained in a Triple.

    Triple.Extra.sum ( 1, 2, 3 )
        --> 6

-}
sum : ( number, number, number ) -> number
sum ( a, b, c ) =
    a + b + c


{-| Similar to List.sum but for Triples instead of lists. Multiplies together three
numbers contained in a Triple

    Triple.Extra.product ( 1, 2, 3 )
        --> 6

-}
product : ( number, number, number ) -> number
product ( a, b, c ) =
    a * b * c


{-| Similar to List.sort but for Triples instead of lists. Sort values contained
in a Triple from lowest to highest

    Triple.Extra.sort ( 2, 1, 3 )
        --> ( 1, 2, 3 )

-}
sort : ( comparable, comparable, comparable ) -> ( comparable, comparable, comparable )
sort =
    sortBy identity


{-| Similar to List.sortBy but for Triples instead of lists. Sort values
contained in a Triple by first converting all values to a `comparable`. The
values are sorted lowest to highest

    ( "mouse", "cat", "elephant" )
        |> Triple.Extra.sortBy String.length
        --> ( "cat", "mouse", "elephant" )

-}
sortBy : (a -> comparable) -> ( a, a, a ) -> ( a, a, a )
sortBy toComparable =
    sortWith (Order.Extra.byField toComparable)


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


{-| Creates a triple from the first three elements of a list.

    Triple.Extra.fromListHead [ 1, 2, 3, 4 ]
        --> Just ( 1, 2, 3 )

    Triple.Extra.fromListHead [ 1, 2 ]
        --> Nothing

-}
fromListHead : List a -> Maybe ( a, a, a )
fromListHead lst =
    case lst of
        a :: b :: c :: _ ->
            Just ( a, b, c )

        _ ->
            Nothing



-- Swizzling ---------------------------------------------------------------------


{-| Retrieves the first and first elements of the triple.
-}
getAA : ( a, b, c ) -> ( a, a )
getAA ( a, _, _ ) =
    ( a, a )


{-| Retrieves the first and second elements of the triple.
-}
getAB : ( a, b, c ) -> ( a, b )
getAB ( a, b, _ ) =
    ( a, b )


{-| Retrieves the first and third elements of the triple.
-}
getAC : ( a, b, c ) -> ( a, c )
getAC ( a, _, c ) =
    ( a, c )


{-| Retrieves the second and first elements of the triple.
-}
getBA : ( a, b, c ) -> ( b, a )
getBA ( a, b, _ ) =
    ( b, a )


{-| Retrieves the second and second elements of the triple.
-}
getBB : ( a, b, c ) -> ( b, b )
getBB ( _, b, _ ) =
    ( b, b )


{-| Retrieves the second and third elements of the triple.
-}
getBC : ( a, b, c ) -> ( b, c )
getBC ( _, b, c ) =
    ( b, c )


{-| Retrieves the third and first elements of the triple.
-}
getCA : ( a, b, c ) -> ( c, a )
getCA ( a, _, c ) =
    ( c, a )


{-| Retrieves the third and second elements of the triple.
-}
getCB : ( a, b, c ) -> ( c, b )
getCB ( _, b, c ) =
    ( c, b )


{-| Retrieves the third and third elements of the triple.
-}
getCC : ( a, b, c ) -> ( c, c )
getCC ( _, _, c ) =
    ( c, c )



---- NOTE: I codegenned the following functions using this script:
-- ch = ['A', 'B', 'C']
-- toVar a = String.toLower (String.fromList [a])
-- List.concatMap (\a ->
--     List.concatMap (\b ->
--         List.map (\c ->
--             let
--                 binding bind =
--                     if toVar a == bind || toVar b == bind || toVar c == bind then
--                         bind
--                     else
--                         "_"
--                 toWord n =
--                     if n == 'A' then
--                         "first"
--                     else if n == 'B' then
--                         "second"
--                     else
--                         "third"
--             in
--             "{-| Retrieves the " ++ toWord a ++ ", " ++ toWord b ++ " and " ++ toWord c ++ " elements of the triple.\n\n    Triple.Extra.get" ++ String.fromList [a,b,c] ++ " ( 'A', 'B', 'C' )\n        --> ( '" ++ String.fromChar a ++ "', '" ++ String.fromChar b ++ "', '" ++ String.fromChar c ++ "' ) -}\nget" ++ String.fromList [a,b,c] ++ " : (a,b,c) -> (" ++ toVar a ++ ", " ++ toVar b ++ ", " ++ toVar c ++
--             ")\nget" ++ String.fromList [a,b,c] ++ " ( " ++ binding "a" ++ ", " ++ binding "b" ++ ", " ++ binding "c" ++ ") = (" ++ toVar a ++ ", " ++ toVar b ++ ", " ++ toVar c ++ ")"
--         ) ch)
--     ch)
-- ch
-- |> String.join "\n\n"


{-| Retrieves the first, first and first elements of the triple.

    Triple.Extra.getAAA ( 'A', 'B', 'C' )
        --> ( 'A', 'A', 'A' )

-}
getAAA : ( a, b, c ) -> ( a, a, a )
getAAA ( a, _, _ ) =
    ( a, a, a )


{-| Retrieves the first, first and second elements of the triple.

    Triple.Extra.getAAB ( 'A', 'B', 'C' )
        --> ( 'A', 'A', 'B' )

-}
getAAB : ( a, b, c ) -> ( a, a, b )
getAAB ( a, b, _ ) =
    ( a, a, b )


{-| Retrieves the first, first and third elements of the triple.

    Triple.Extra.getAAC ( 'A', 'B', 'C' )
        --> ( 'A', 'A', 'C' )

-}
getAAC : ( a, b, c ) -> ( a, a, c )
getAAC ( a, _, c ) =
    ( a, a, c )


{-| Retrieves the first, second and first elements of the triple.

    Triple.Extra.getABA ( 'A', 'B', 'C' )
        --> ( 'A', 'B', 'A' )

-}
getABA : ( a, b, c ) -> ( a, b, a )
getABA ( a, b, _ ) =
    ( a, b, a )


{-| Retrieves the first, second and second elements of the triple.

    Triple.Extra.getABB ( 'A', 'B', 'C' )
        --> ( 'A', 'B', 'B' )

-}
getABB : ( a, b, c ) -> ( a, b, b )
getABB ( a, b, _ ) =
    ( a, b, b )


{-| Retrieves the first, second and third elements of the triple.

    Triple.Extra.getABC ( 'A', 'B', 'C' )
        --> ( 'A', 'B', 'C' )

-}
getABC : ( a, b, c ) -> ( a, b, c )
getABC ( a, b, c ) =
    ( a, b, c )


{-| Retrieves the first, third and first elements of the triple.

    Triple.Extra.getACA ( 'A', 'B', 'C' )
        --> ( 'A', 'C', 'A' )

-}
getACA : ( a, b, c ) -> ( a, c, a )
getACA ( a, _, c ) =
    ( a, c, a )


{-| Retrieves the first, third and second elements of the triple.

    Triple.Extra.getACB ( 'A', 'B', 'C' )
        --> ( 'A', 'C', 'B' )

-}
getACB : ( a, b, c ) -> ( a, c, b )
getACB ( a, b, c ) =
    ( a, c, b )


{-| Retrieves the first, third and third elements of the triple.

    Triple.Extra.getACC ( 'A', 'B', 'C' )
        --> ( 'A', 'C', 'C' )

-}
getACC : ( a, b, c ) -> ( a, c, c )
getACC ( a, _, c ) =
    ( a, c, c )


{-| Retrieves the second, first and first elements of the triple.

    Triple.Extra.getBAA ( 'A', 'B', 'C' )
        --> ( 'B', 'A', 'A' )

-}
getBAA : ( a, b, c ) -> ( b, a, a )
getBAA ( a, b, _ ) =
    ( b, a, a )


{-| Retrieves the second, first and second elements of the triple.

    Triple.Extra.getBAB ( 'A', 'B', 'C' )
        --> ( 'B', 'A', 'B' )

-}
getBAB : ( a, b, c ) -> ( b, a, b )
getBAB ( a, b, _ ) =
    ( b, a, b )


{-| Retrieves the second, first and third elements of the triple.

    Triple.Extra.getBAC ( 'A', 'B', 'C' )
        --> ( 'B', 'A', 'C' )

-}
getBAC : ( a, b, c ) -> ( b, a, c )
getBAC ( a, b, c ) =
    ( b, a, c )


{-| Retrieves the second, second and first elements of the triple.

    Triple.Extra.getBBA ( 'A', 'B', 'C' )
        --> ( 'B', 'B', 'A' )

-}
getBBA : ( a, b, c ) -> ( b, b, a )
getBBA ( a, b, _ ) =
    ( b, b, a )


{-| Retrieves the second, second and second elements of the triple.

    Triple.Extra.getBBB ( 'A', 'B', 'C' )
        --> ( 'B', 'B', 'B' )

-}
getBBB : ( a, b, c ) -> ( b, b, b )
getBBB ( _, b, _ ) =
    ( b, b, b )


{-| Retrieves the second, second and third elements of the triple.

    Triple.Extra.getBBC ( 'A', 'B', 'C' )
        --> ( 'B', 'B', 'C' )

-}
getBBC : ( a, b, c ) -> ( b, b, c )
getBBC ( _, b, c ) =
    ( b, b, c )


{-| Retrieves the second, third and first elements of the triple.

    Triple.Extra.getBCA ( 'A', 'B', 'C' )
        --> ( 'B', 'C', 'A' )

-}
getBCA : ( a, b, c ) -> ( b, c, a )
getBCA ( a, b, c ) =
    ( b, c, a )


{-| Retrieves the second, third and second elements of the triple.

    Triple.Extra.getBCB ( 'A', 'B', 'C' )
        --> ( 'B', 'C', 'B' )

-}
getBCB : ( a, b, c ) -> ( b, c, b )
getBCB ( _, b, c ) =
    ( b, c, b )


{-| Retrieves the second, third and third elements of the triple.

    Triple.Extra.getBCC ( 'A', 'B', 'C' )
        --> ( 'B', 'C', 'C' )

-}
getBCC : ( a, b, c ) -> ( b, c, c )
getBCC ( _, b, c ) =
    ( b, c, c )


{-| Retrieves the third, first and first elements of the triple.

    Triple.Extra.getCAA ( 'A', 'B', 'C' )
        --> ( 'C', 'A', 'A' )

-}
getCAA : ( a, b, c ) -> ( c, a, a )
getCAA ( a, _, c ) =
    ( c, a, a )


{-| Retrieves the third, first and second elements of the triple.

    Triple.Extra.getCAB ( 'A', 'B', 'C' )
        --> ( 'C', 'A', 'B' )

-}
getCAB : ( a, b, c ) -> ( c, a, b )
getCAB ( a, b, c ) =
    ( c, a, b )


{-| Retrieves the third, first and third elements of the triple.

    Triple.Extra.getCAC ( 'A', 'B', 'C' )
        --> ( 'C', 'A', 'C' )

-}
getCAC : ( a, b, c ) -> ( c, a, c )
getCAC ( a, _, c ) =
    ( c, a, c )


{-| Retrieves the third, second and first elements of the triple.

    Triple.Extra.getCBA ( 'A', 'B', 'C' )
        --> ( 'C', 'B', 'A' )

-}
getCBA : ( a, b, c ) -> ( c, b, a )
getCBA ( a, b, c ) =
    ( c, b, a )


{-| Retrieves the third, second and second elements of the triple.

    Triple.Extra.getCBB ( 'A', 'B', 'C' )
        --> ( 'C', 'B', 'B' )

-}
getCBB : ( a, b, c ) -> ( c, b, b )
getCBB ( _, b, c ) =
    ( c, b, b )


{-| Retrieves the third, second and third elements of the triple.

    Triple.Extra.getCBC ( 'A', 'B', 'C' )
        --> ( 'C', 'B', 'C' )

-}
getCBC : ( a, b, c ) -> ( c, b, c )
getCBC ( _, b, c ) =
    ( c, b, c )


{-| Retrieves the third, third and first elements of the triple.

    Triple.Extra.getCCA ( 'A', 'B', 'C' )
        --> ( 'C', 'C', 'A' )

-}
getCCA : ( a, b, c ) -> ( c, c, a )
getCCA ( a, _, c ) =
    ( c, c, a )


{-| Retrieves the third, third and second elements of the triple.

    Triple.Extra.getCCB ( 'A', 'B', 'C' )
        --> ( 'C', 'C', 'B' )

-}
getCCB : ( a, b, c ) -> ( c, c, b )
getCCB ( _, b, c ) =
    ( c, c, b )


{-| Retrieves the third, third and third elements of the triple.

    Triple.Extra.getCCC ( 'A', 'B', 'C' )
        --> ( 'C', 'C', 'C' )

-}
getCCC : ( a, b, c ) -> ( c, c, c )
getCCC ( _, _, c ) =
    ( c, c, c )
