module Changes exposing (..)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node
import Review.Fix exposing (Fix)


renames : List ( String, String )
renames =
    [ ( "Basics.Extra.swap", "Tuple.Extra.flip" )
    , ( "Basics.Extra.atMost", "min" )
    , ( "Basics.Extra.atLeast", "max" )
    , ( "Basics.Extra.fractionalModBy", "Float.Extra.modBy" )
    , ( "Basics.Extra.orderBy", "Order.Extra.breakTies" )
    , ( "Basics.Extra.toOrder", "Order.Extra.byField" )
    ]


customRename : List String -> String -> Maybe ( List String, String )
customRename mod name =
    if mod == [ "Basics", "Extra" ] && name == "toOrderDesc" then
        Just ( [ "Order", "Extra" ], "(Order.Extra.byField >> Order.Extra.reverse)" )

    else
        Nothing
