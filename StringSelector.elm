module StringSelector exposing (model, Model, view, update, Action)

import Array exposing (Array)
import Array
import Html exposing (Attribute, div, button, span, text, select, option)
import Html.Attributes exposing (style, selected)
import Html.Events exposing (targetValue, on, onClick, onInput)
import Notes


type alias Model =
    { selectedString : String }


model : Model
model =
    { selectedString = "C" }


view : Model -> Html.Html Action
view model =
    div [ style [ ( "display", "flex" ) ] ]
        [ viewAddString model
        , viewConfirmAddString
        ]


viewAddString model =
    let
        renderCandidate note =
            option [ selected (model.selectedString == note) ] [ text note ]

        stringCandidates =
            List.map renderCandidate <| Array.toList Notes.notes
    in
        div []
            [ select [ onInput SelectString ] stringCandidates
            ]


viewConfirmAddString =
    button [ onClick ConfirmAddString ] [ text "Add string" ]


viewString : String -> Html.Html Action
viewString string =
    let
        startIndex =
            Notes.noteIndex string

        notesList =
            List.map (Notes.getNote << (\a -> a + startIndex)) (List.range 0 12)

        viewNote note =
            div [ style [ ( "width", "40px" ), ( "text-align", "center" ) ] ] [ text note ]
    in
        div [ style [ ( "display", "flex" ) ] ] (List.map viewNote notesList)


type Action
    = ConfirmAddString
    | SelectString String


update action model =
    case action of
        ConfirmAddString ->
            ( model, Just model.selectedString )

        SelectString s ->
            ( { model | selectedString = s }, Nothing )
