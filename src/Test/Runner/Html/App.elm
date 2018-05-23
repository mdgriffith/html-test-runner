module Test.Runner.Html.App
    exposing
        ( Model
        , Msg(..)
        , init
        , present
        , update
        )

import Expect exposing (Expectation)
import Process
import Random
import Task
import Test exposing (Test)
import Test.Runner.Exploration as Runner
import Test.Runner.Html.View as View
import Time


type Model
    = NotStarted (Maybe Random.Seed) Int Test
    | Started Time.Posix Time.Posix Runner.Status


type Msg
    = Dispatch Time.Posix


dispatch : Cmd Msg
dispatch =
    Process.sleep 0
        |> Task.andThen (\_ -> Time.now)
        |> Task.perform Dispatch


start : Int -> Test -> Random.Seed -> Runner.Status
start runs test seed =
    Runner.fromTest runs seed test
        |> Runner.step


init : Int -> Maybe Random.Seed -> Test -> ( Model, Cmd Msg )
init runs maybeSeed test =
    ( NotStarted maybeSeed runs test, dispatch )


update : Msg -> Model -> ( Model, Cmd Msg )
update (Dispatch now) model =
    case model of
        NotStarted Nothing runs test ->
            ( Time.toMillis Time.utc now
                |> Random.initialSeed
                |> start runs test
                |> Started now now
            , dispatch
            )

        NotStarted (Just seed) runs test ->
            ( Started now now (start runs test seed)
            , dispatch
            )

        Started startTime _ (Runner.Running { next }) ->
            ( Started startTime now (Runner.step next)
            , dispatch
            )

        Started startTime _ status ->
            ( Started startTime now status
            , Cmd.none
            )


present : Model -> View.Model
present model =
    case model of
        NotStarted _ _ _ ->
            Nothing

        Started startTime now status ->
            let
                diff =
                    Time.toMillis Time.utc now - Time.toMillis Time.utc startTime
            in
            Just
                ( Time.millisToPosix diff
                , status
                )
