module FretsSelector exposing (model, view, Action, update, Model)

import Array exposing (Array)
import Array
import Html exposing (Attribute, div, p, button, span, text, select, option)
import Html.Attributes exposing (style, selected)
import Html.Events exposing (targetValue, on, onClick)
import Notes


type alias Model =
    { startFret : Int, endFret : Int }


model : Model
model =
    { startFret = 0, endFret = 12 }


view : Model -> Html.Html Action
view model =
    div []
        [ div [ style [ ( "display", "flex" ) ] ]
            [ div [ style [ ( "margin", "10px" ), ( "text-align", "center" ) ] ]
                [ text "Left"
                , div [ style [ ( "display", "flex" ), ( "width", "80px" ), ( "justify-content", "space-around" ) ] ]
                    [ div [] [ button [ onClick ShrinkLeft ] [ text "<-" ] ]
                    , div [] [ button [ onClick GrowLeft ] [ text "->" ] ]
                    ]
                ]
            , div [ style [ ( "margin", "10px" ), ( "text-align", "center" ) ] ]
                [ text "Right"
                , div [ style [ ( "display", "flex" ), ( "width", "80px" ), ( "justify-content", "space-around" ) ] ]
                    [ div [] [ button [ onClick ShrinkRight ] [ text "<-" ] ]
                    , div [] [ button [ onClick GrowRight ] [ text "->" ] ]
                    ]
                ]
            ]
        ]


type Action
    = GrowLeft
    | ShrinkLeft
    | GrowRight
    | ShrinkRight


update action model =
    let
        validate newModel =
            if newModel.endFret > newModel.startFret && newModel.startFret >= 0 then
                newModel
            else
                model

        nextModel =
            case action of
                GrowRight ->
                    { model | endFret = model.endFret + 1 }

                ShrinkRight ->
                    { model | endFret = model.endFret - 1 }

                GrowLeft ->
                    { model | startFret = model.startFret + 1 }

                ShrinkLeft ->
                    { model | startFret = model.startFret - 1 }
    in
        validate nextModel
