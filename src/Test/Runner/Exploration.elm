module Test.Runner.Exploration
    exposing
        ( Failure
        , Reason(..)
        , Runner
        , Status(..)
        , formatFailure
        , fromTest
        , step
        , toString
        )

import Expect
import Random
import Test
import Test.Runner
import Test.Runner.Failure


toString : Runner -> String
toString (Runner internals) =
    let
        keyValue key value =
            "    " ++ key ++ ": " ++ value
    in
    String.join "\n"
        [ keyValue "passed" (String.fromInt internals.passed)
        , keyValue "todos" ("\n        [ " ++ String.join "        \n" (List.map failureToString internals.todos) ++ "]")
        , keyValue "failures" ("\n     [ " ++ String.join "        \n" (List.map failureToString internals.failures) ++ "]")
        , keyValue "incomplete"
            (case internals.incomplete of
                Nothing ->
                    "Nothing"

                Just reason ->
                    "Just "
                        ++ (case reason of
                                Skip ->
                                    "Skip"

                                Only ->
                                    "Only"

                                Custom label ->
                                    "Custom " ++ label
                           )
            )
        , keyValue "queue"
            (case internals.queue of
                [] ->
                    "[]"

                q ->
                    String.join " " (List.map (.labels >> String.join ", ") q)
            )
        ]


failureToString : Failure -> String
failureToString (Failure labels messages) =
    "failure: " ++ String.join ", " labels ++ ", reasons: " ++ String.join ", " (List.map .description messages)


type Runner
    = Runner Internals


type alias Internals =
    { passed : Int
    , failures : List Failure
    , todos : List Failure
    , queue : List Test.Runner.Runner
    , incomplete : Maybe Reason
    }


type Status
    = Running
        { passed : Int
        , remaining : Int
        , failures : List Failure
        , next : Runner
        }
    | Pass Int
    | Fail Int (List Failure)
    | Todo Int (List Failure)
    | Incomplete Int Reason


type Reason
    = Skip
    | Only
    | Custom String


type Failure
    = Failure
        (List String)
        (List
            { given : Maybe String
            , description : String
            , reason : Test.Runner.Failure.Reason
            }
        )


fromTest : Int -> Random.Seed -> Test.Test -> Runner
fromTest runs seed test =
    let
        new queue incomplete =
            Runner
                { passed = 0
                , failures = []
                , todos = []
                , queue = queue
                , incomplete = incomplete
                }
    in
    case Test.Runner.fromTest runs seed test of
        Test.Runner.Plain queue ->
            new queue Nothing

        Test.Runner.Only queue ->
            new queue (Just Only)

        Test.Runner.Skipping queue ->
            new queue (Just Skip)

        Test.Runner.Invalid reason ->
            new [] (Just (Custom reason))


formatFailure :
    (String -> a)
    -> (String -> a)
    -> Failure
    ->
        ( List a
        , List
            { given : Maybe String
            , description : String
            , reason : Test.Runner.Failure.Reason
            }
        )
formatFailure formatFirst formatLast (Failure labels errors) =
    ( Test.Runner.formatLabels formatFirst formatLast labels, errors )


step : Runner -> Status
step (Runner internals) =
    case ( internals.failures, internals.queue ) of
        ( [], [] ) ->
            case internals.incomplete of
                Nothing ->
                    case internals.todos of
                        [] ->
                            Pass internals.passed

                        todos ->
                            Todo internals.passed todos

                Just incompleteReason ->
                    Incomplete internals.passed incompleteReason

        ( failures, [] ) ->
            Fail internals.passed failures

        ( _, next :: queue ) ->
            next.run ()
                |> fromExpectation { internals | queue = queue } next.labels


fromExpectation : Internals -> List String -> List Expect.Expectation -> Status
fromExpectation internals labels expectations =
    let
        ( todos, failures ) =
            List.foldr partition ( [], [] ) expectations

        partition e =
            case Test.Runner.getFailureReason e of
                Just reason ->
                    if Test.Runner.isTodo e then
                        Tuple.mapFirst ((::) reason)
                    else
                        Tuple.mapSecond ((::) reason)

                Nothing ->
                    identity
    in
    if not <| List.isEmpty failures then
        toRunning
            { internals
                | failures = internals.failures ++ [ Failure labels failures ]
            }
    else if not <| List.isEmpty todos then
        toRunning
            { internals
                | todos = internals.todos ++ [ Failure labels todos ]
            }
    else
        toRunning
            { internals
                | passed = internals.passed + 1
            }


toRunning : Internals -> Status
toRunning internals =
    Running
        { passed = internals.passed
        , remaining = List.length internals.queue
        , failures = internals.failures
        , next = Runner internals
        }
