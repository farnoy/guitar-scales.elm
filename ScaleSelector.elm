module ScaleSelector exposing (init, Model, view, update, Action)

import Array exposing (Array)
import Array
import Dict exposing (Dict)
import Dict
import Html exposing (Attribute, div, button, span, text, select, option)
import Html.Attributes exposing (style, selected, value)
import Html.Events exposing (targetValue, on, onClick, onInput)
import Notes
import String


type alias Model =
    { selectedKey : String
    , selectedScale : String
    }


init : Model
init =
    { selectedKey = "C", selectedScale = "Major" }


view : Model -> Html.Html Action
view model =
    let
        renderScaleOption ( scale, _ ) =
            option [ value scale, selected (model.selectedScale == scale) ] [ text scale ]

        scalesInput =
            select [ onInput SelectScale ] <|
                List.map renderScaleOption (Dict.toList Notes.scales)

        renderKeyOption key =
            option [ value key, selected (model.selectedKey == key) ] [ text key ]

        keyInput =
            select [ onInput SelectKey ] <|
                (List.map renderKeyOption <| Array.toList Notes.notes)
    in
        div []
            [ keyInput
            , scalesInput
            ]


type Action
    = SelectKey String
    | SelectScale String


update action model =
    case action of
        SelectKey k ->
            { model | selectedKey = k }

        SelectScale s ->
            { model | selectedScale = s }
