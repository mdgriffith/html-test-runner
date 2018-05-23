module HtmlRunnerExample exposing (..)

{-| HOW TO RUN THIS EXAMPLE

1.  Run elm-reactor from the same directory as your tests' elm.json. (For example, if you have tests/elm.json, then cd into tests/ and
    run elm-reactor.)
2.  Visit <http://localhost:8000> and bring up this file.

-}

import Char
import Expect
import Fuzz exposing (..)
import String
import Test exposing (..)
import Test.Runner.Html


main : Test.Runner.Html.TestProgram
main =
    [ testWithoutNums
    , testOxfordify
    , noDescription
    , testExpectations
    , testFailingFuzzTests
    , testFuzz
    ]
        |> concat
        |> Test.Runner.Html.run


withoutNums : String -> String
withoutNums =
    String.filter (\ch -> not (Char.isDigit ch || ch == '.'))


testWithoutNums : Test
testWithoutNums =
    describe "withoutNums"
        [ fuzzWith { runs = 100 } (tuple3 ( string, float, string )) "adding numbers to strings has no effect" <|
            \( prefix, num, suffix ) ->
                withoutNums (prefix ++ String.fromFloat num ++ suffix)
                    |> Expect.equal (withoutNums (prefix ++ suffix))
        ]


testExpectations : Test
testExpectations =
    describe "basic expectations"
        [ test "this should succeed" <|
            \() ->
                "blah"
                    |> Expect.equal "blah"
        , test "this should fail" <|
            \() ->
                "something"
                    |> Expect.equal "someting else"
        , test "another failure" <|
            \() ->
                "forty-two"
                    |> Expect.equal "forty-three"
        ]



{- After this point, we're really just showing that Richard's proposed API compiles. -}


{-| stubbed function under test
-}
oxfordify : a -> b -> c -> String
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


noDescription : Test
noDescription =
    test "" <|
        \() ->
            Expect.equal "No description" "Whatsoever!"


testFuzz : Test
testFuzz =
    describe "fuzzing"
        [ fuzz2 string string "empty list etc" <|
            \name punctuation ->
                oxfordify "This sentence is empty" "." []
                    |> Expect.equal ""
                    |> Expect.onFail "given an empty list, did not return an empty string"
        , fuzz2 string string "further testing" <|
            \name punctuation ->
                oxfordify "This sentence contains " "." [ "one item" ]
                    |> Expect.equal "This sentence contains one item."
        , fuzz2 string string "custom onFail here" <|
            \name punctuation ->
                oxfordify "This sentence contains " "." [ "one item", "two item" ]
                    |> Expect.equal "This sentence contains one item and two item."
                    |> Expect.onFail "given an empty list, did not return an empty string"
        , fuzz2 string string "This is a test." <|
            \name punctuation ->
                oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                    |> Expect.equal "This sentence contains one item, two item, and three item."
                    |> Expect.onFail "given a list of length 3, did not return an oxford-style sentence"
        ]


testFailingFuzzTests : Test
testFailingFuzzTests =
    describe "the first element in this fuzz tuple"
        [ fuzz2 string string "is always \"foo\"" <|
            \str1 str2 ->
                str1
                    |> Expect.equal "foo"
        ]


testOxfordify : Test
testOxfordify =
    describe "oxfordify"
        [ describe "given an empty sentence"
            [ test "returns an empty string" <|
                \() ->
                    oxfordify "This sentence is empty" "." []
                        |> Expect.equal ""
            ]
        , describe "given a sentence with one item"
            [ test "still contains one item" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item" ]
                        |> Expect.equal "This sentence contains one item."
            ]
        , describe "given a sentence with multiple items"
            [ test "returns an oxford-style sentence" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item", "two item" ]
                        |> Expect.equal "This sentence contains one item and two item."
            , test "returns an oxford-style sentence" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                        |> Expect.equal "This sentence contains one item, two item, and three item."
            ]
        ]


testShrinkables : Test
testShrinkables =
    describe "Some tests that should fail and produce shrunken values"
        [ describe "a randomly generated integer"
            [ fuzz int "is for sure exactly 0" <| Expect.equal 0
            , fuzz int "is <42" <| Expect.lessThan 42
            , fuzz int "is also >42" <| Expect.greaterThan 42
            ]
        , describe "a randomly generated string"
            [ fuzz string "equals its reverse" <|
                \str ->
                    Expect.equal str (String.reverse str)
            ]
        ]
