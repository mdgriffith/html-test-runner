module View exposing (view)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import String
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Test.Runner.Exploration as Runner
import Test.Runner.Failure as Failure
import Test.Runner.Html.View as View
import Time


view : View.Model -> Html a
view model =
    Element.viewport styleSheet (app model)


type Styles
    = None
    | App
    | Header Palette
    | Description Palette


type Palette
    = Primary
    | Secondary
    | Accent
    | Background
    | Good
    | Bad
    | Warning


rgb255 r g b =
    Style.rgb (r / 255) (g / 255) (b / 255)


color : Palette -> Color
color palette =
    case palette of
        Primary ->
            rgb255 41 60 75

        Secondary ->
            -- gray color on elm blog is rgb 221 221 221 but it doesn't meet
            -- accessibility standards for contrast http://webaim.org/resources/contrastchecker/
            rgb255 84 84 84

        Accent ->
            rgb255 96 181 204

        Background ->
            rgb255 255 255 255

        Good ->
            rgb255 0 100 0

        Bad ->
            rgb255 179 0 0

        Warning ->
            rgb255 122 67 0


withColor :
    (Palette -> class)
    -> List (Property class variation)
    -> List (Style class variation)
withColor toStyle attributes =
    let
        withColorHelp p =
            style
                (toStyle p)
                (Color.text (color p) :: attributes)
    in
    List.map withColorHelp
        [ Primary
        , Secondary
        , Accent
        , Background
        , Good
        , Bad
        , Warning
        ]


styleSheet : StyleSheet Styles variation
styleSheet =
    [ [ style None
            []
      , style App
            [ Color.text (color Primary)
            , Color.border (color Accent)
            , Font.typeface
                [ Font.font "Source Sans Pro"
                , Font.font "Trebuchet MS"
                , Font.font "Lucida Grande"
                , Font.font "Bitstream Vera Sans"
                , Font.font "Helvetica Neue"
                , Font.sansSerif
                ]
            , Border.top 8
            ]
      ]
    , withColor Description
        []
    , withColor Header
        [ Font.size 24
        , Font.bold
        ]
    ]
        |> List.concat
        |> Style.styleSheet


app : View.Model -> Element Styles variations msg
app model =
    let
        wrapper nested =
            row App
                [ padding 20 ]
                [ el None [ width fill ] empty
                , el None [ width (px 960) ] nested
                , el None [ width fill ] empty
                ]
    in
    wrapper <|
        case model of
            Nothing ->
                "Loading Tests..."
                    |> text
                    |> header (Header Primary) [ paddingBottom 24 ]
                    |> summary []

            Just ( duration, Runner.Pass passed ) ->
                ( Good, "Test Run Passed" )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Todo passed failures ) ->
                ( Warning, "Test Run Incomplete: TODO's remaining" )
                    |> finished duration passed failures
                    |> summary failures

            Just ( duration, Runner.Incomplete passed Runner.Only ) ->
                ( Warning, "Test Run Incomplete: Test.only was used" )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Incomplete passed Runner.Skip ) ->
                ( Warning, "Test Run Incomplete: Test.skip was used" )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Incomplete passed (Runner.Custom reason) ) ->
                ( Warning, "Test Run Incomplete: " ++ reason )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Fail passed failures ) ->
                ( Bad, "Test Run Failed" )
                    |> finished duration passed failures
                    |> summary failures

            Just ( duration, Runner.Running { passed, failures, remaining } ) ->
                running (passed + List.length failures) remaining
                    |> summary failures


running : Int -> Int -> Element Styles variations msg
running completed remaining =
    column None
        []
        [ header (Header Primary) [ paddingBottom 24 ] (text "Running Tests...")
        , row None [] [ text (String.fromInt completed ++ " completed") ]
        , row None [] [ text (String.fromInt remaining ++ " remaining") ]
        ]


finished : Time.Posix -> Int -> List a -> ( Palette, String ) -> Element Styles variation msg
finished duration passed failures ( headlineColor, headlineText ) =
    column None
        []
        [ header (Header headlineColor) [ paddingBottom 24 ] (text headlineText)
        , row None
            []
            [ table None
                [ spacing 10 ]
                [ [ bold "Duration", bold "Passed", bold "Failed" ]
                , [ text (formattedDuration duration)
                  , text (String.fromInt passed)
                  , text (String.fromInt (List.length failures))
                  ]
                ]
            ]
        ]


summary : List Runner.Failure -> Element Styles variation msg -> Element Styles variation msg
summary failures message =
    column None
        []
        [ wrappedRow None [] [ message ]
        , wrappedRow None [] [ allFailures failures ]
        ]


allFailures : List Runner.Failure -> Element Styles variation msg
allFailures failures =
    List.map (oneFailure >> node "li") failures
        |> column None
            [ spacing 10
            , padding 10
            ]
        |> node "ol"


oneFailure : Runner.Failure -> Element Styles variations msg
oneFailure failure =
    let
        ( labels, expectations ) =
            Runner.formatFailure
                (coloredLabel '↓' Secondary)
                (coloredLabel '✗' Bad)
                failure

        inContext ({ given } as reason) =
            column None
                [ spacing 10 ]
                [ wrappedRow None [] [ whenJust given givenCode ]
                , paragraph None [ width (px 800) ] [ code None (viewReason reason) ]
                ]
    in
    el None
        [ inlineStyle "display" "list-item"
        , inlineStyle "margin" "10px"
        , inlineStyle "padding" "10px"
        ]
    <|
        column None
            [ spacing 5 ]
            (labels ++ [ spacer 3 ] ++ List.map inContext expectations)


viewReason : { a | reason : Failure.Reason, description : String } -> String
viewReason { reason, description } =
    case reason of
        Failure.Custom ->
            description

        Failure.Equality one two ->
            description ++ " " ++ one ++ " " ++ two

        Failure.Comparison one two ->
            description ++ " " ++ one ++ " " ++ two

        Failure.ListDiff expected actual ->
            "expected\n"
                ++ String.join "    \n" expected
                ++ "actual\n"
                ++ String.join "    \n" actual

        Failure.CollectionDiff { expected, actual, extra, missing } ->
            String.join "\n"
                [ formatKeyValue "expected" expected
                , formatKeyValue "actual" actual
                , formatKeyValue "extra" (String.join ", " extra)
                , formatKeyValue "missing" (String.join ", " missing)
                ]

        Failure.TODO ->
            description

        Failure.Invalid _ ->
            description


formatKeyValue : String -> String -> String
formatKeyValue key val =
    key ++ ": " ++ val


givenCode : String -> Element Styles variations msg
givenCode value =
    code None ("Given " ++ value)


coloredLabel : Char -> Palette -> String -> Element Styles variation msg
coloredLabel char textColor str =
    column (Description textColor)
        []
        [ text (String.cons char (String.cons ' ' str)) ]


formattedDuration : Time.Posix -> String
formattedDuration time =
    String.fromInt (Time.toMillis Time.utc time) ++ " ms"


code : style -> String -> Element style variations msg
code style str =
    node "pre" <|
        el style
            [ inlineStyle "white-space" "pre-wrap"
            , inlineStyle "font-family" "monospace"
            ]
            (text str)
