module Cmd.Extra exposing
    ( perform, attempt, maybe, fromResult, fromMaybe
    , pure, with, add, withTrigger, addTrigger, addIf, addTriggerMaybe, addMaybe
    )

{-| Extra functions for working with Cmds.


# Constructors

@docs perform, attempt, maybe, fromResult, fromMaybe


# Chaining in update

@docs pure, with, add, withTrigger, addTrigger, addIf, addTriggerMaybe, addMaybe

-}

import Task



-- Constructors


{-| Cmd constructor.
Useful when you want to artificially emit Cmd from update function.

    performed : Cmd String
    performed =
        perform "foo"

"real world" example:

    type alias Model =
        ()

    type Msg
        = Fire
        | FireRockets

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg () =
        case msg of
            Fire ->
                ( (), perform FireRockets )

            FireRockets ->
                Debug.crash "World ended:("

-}
perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed


{-| Similar to perform but takes `Result msg` and performs action only on `Ok`.

    attempted : Cmd String
    attempted =
      attempt <| Ok "I'm fine"

    attempt (Err "Failed") == Cmd.none
    --> True

-}
attempt : Result x msg -> Cmd msg
attempt =
    Result.withDefault Cmd.none << Result.map perform


{-| Similar to attempt but works with `Maybe` instead

    maybeCmd : Cmd Int
    maybeCmd =
        maybe <| Just 1

    maybe Nothing == Cmd.none
    --> True

-}
maybe : Maybe msg -> Cmd msg
maybe maybeMsg =
    Maybe.map perform maybeMsg
        |> Maybe.withDefault Cmd.none


{-| Construct from Result.

    resultCmd : Cmd (Result Never Int)
    resultCmd =
      fromResult identity (Ok 1)

    fromResult identity (Err ())
    --> Cmd.none

-}
fromResult : (a -> msg) -> Result x a -> Cmd msg
fromResult const res =
    Result.map const res
        |> attempt


{-| Construct from Maybe.

    maybeCmd : Cmd (Maybe Int)
    maybeCmd =
      identity (Just 1)

    fromMaybe identity Nothing
    --> Cmd.none

-}
fromMaybe : (a -> msg) -> Maybe a -> Cmd msg
fromMaybe const m =
    Maybe.map const m
        |> maybe



-- Chaining in update


{-| Creates pair `model` with `Cmd.none`

    pair : ( String, Cmd msg )
    pair = pure "foo"

    pair
      |> Tuple.second
      |> ((==) Cmd.none)
    --> True

-}
pure : model -> ( model, Cmd msg )
pure model =
    ( model, Cmd.none )


{-| Add Cmd to model to create a pair.
-}
with : Cmd msg -> model -> ( model, Cmd msg )
with cmd model =
    ( model, cmd )


{-| Add new cmd to an existing pair.
-}
add : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
add newCmd ( model, prevCmd ) =
    ( model, Cmd.batch [ newCmd, prevCmd ] )


{-| Trigger Cmd from Msg and create a pair
-}
withTrigger : msg -> model -> ( model, Cmd msg )
withTrigger msg =
    with <| perform msg


{-| Add new trigger of Msg to an existing pair.
-}
addTrigger : msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addTrigger msg =
    add <| perform msg


{-| Add new cmd to an existing pair under a certain condition.

    prevCmd : Cmd String
    prevCmd =
        perform "foo"

    newCmd : Cmd String
    newCmd =
        perform "bar"

    ( "model", prevCmd )
      |> addIf False newCmd
      |> Tuple.second
      |> ((==) prevCmd)
    --> True

-}
addIf : Bool -> Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addIf predicate newCmd ( model, prevCmd ) =
    ( model
    , if predicate then
        Cmd.batch [ newCmd, prevCmd ]

      else
        prevCmd
    )


{-| Add new cmd to an existing pair based on the Maybe value

    prevCmd : Cmd String
    prevCmd =
        perform "prev"

    ( "model", prevCmd )
      |> addMaybe identity Nothing
      |> Tuple.second
      |> ((==) prevCmd)
    --> True

-}
addMaybe : (a -> Cmd msg) -> Maybe a -> ( model, Cmd msg ) -> ( model, Cmd msg )
addMaybe valueToCmd maybeValue ( model, prevCmd ) =
    ( model
    , Maybe.map (\v -> Cmd.batch [ valueToCmd v, prevCmd ]) maybeValue
        |> Maybe.withDefault prevCmd
    )


{-| `addTrigger` if Just, do nothing if Nothing
-}
addTriggerMaybe : Maybe msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addTriggerMaybe maybeMsg ( model, cmd ) =
    case maybeMsg of
        Just msg ->
            addTrigger msg ( model, cmd )

        Nothing ->
            ( model, cmd )
