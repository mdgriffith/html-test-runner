module Test.Runner.Html.View exposing (..)

import Test.Runner.Exploration as Runner
import Time


type alias Model =
    Maybe ( Time.Posix, Runner.Status )
