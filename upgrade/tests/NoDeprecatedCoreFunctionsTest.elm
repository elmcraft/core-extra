module NoDeprecatedCoreFunctionsTest exposing (all)

import NoDeprecatedCoreFunctions exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoDeprecatedCoreFunctions"
        [ test "should report an error when a deprecated function is used" <|
            \() ->
                """module A exposing (..)

import Basics.Extra

foo : (String, Int)
foo =
    Basics.Extra.swap ( 2, "A" )
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module uses core-extra deprecated functions"
                            , details = [ "These functions are deprecated and will be removed in the next release" ]
                            , under = "Basics.Extra.swap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Basics.Extra
import Tuple.Extra

foo : (String, Int)
foo =
    Tuple.Extra.flip ( 2, "A" )
"""
                        ]
        , test "should report an error for custom renames" <|
            \() ->
                """module A exposing (..)

import Basics.Extra

foo : String -> String -> Order
foo =
    Basics.Extra.toOrderDesc String.length
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module uses core-extra deprecated functions"
                            , details = [ "These functions are deprecated and will be removed in the next release" ]
                            , under = "Basics.Extra.toOrderDesc"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Basics.Extra
import Order.Extra

foo : String -> String -> Order
foo =
    (Order.Extra.byField >> Order.Extra.reverse) String.length
"""
                        ]
        , test "upgrades apply to andMap" <|
            \() ->
                """module A exposing (..)

import Array exposing (Array)
import Array.Extra

foo : Array String
foo =
    Array.Extra.apply (Array.fromList [\\a -> String.fromInt]) (Array.fromList [ 1 ])
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module uses core-extra deprecated functions"
                            , details = [ "These functions are deprecated and will be removed in the next release" ]
                            , under = "Array.Extra.apply (Array.fromList [\\a -> String.fromInt]) (Array.fromList [ 1 ])"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Array exposing (Array)
import Array.Extra

foo : Array String
foo =
    Array.Extra.andMap (Array.fromList [ 1 ]) (Array.fromList [ \\a -> String.fromInt ])
"""
                        ]
        , test "makes error for apply that can't be upgraded" <|
            \() ->
                """module A exposing (..)

import Array exposing (Array)
import Array.Extra

foo : Array String
foo = Array.Extra.apply
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module uses core-extra deprecated functions"
                            , details =
                                [ "Array.Extra.apply is deprecated in favour of Array.Extra.andMap and will be removed."
                                , "However, we cannot automatically upgrade this usage for you, so please update this manually."
                                , "Note that the argument order is different."
                                ]
                            , under = "Array.Extra.apply"
                            }
                        ]
        ]
