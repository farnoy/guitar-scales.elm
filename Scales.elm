module Main exposing (..)

import Array exposing (Array)
import Array
import Debug
import Dict exposing (Dict)
import Dict
import FretsSelector
import Html exposing (Attribute, a, beginnerProgram, div, button, span, ul, li, text, select, option)
import Html.Attributes exposing (style, selected)
import Html.Events exposing (targetValue, on, onClick, onInput)
import Notes
import ScaleSelector
import Set exposing (Set)
import Set
import StringSelector


main =
    beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { strings : Array String
    , stringSelector : StringSelector.Model
    , fretsSelector : FretsSelector.Model
    , scaleSelector : ScaleSelector.Model
    , selectedNotes : Set String
    }


model : Model
model =
    { strings = Array.fromList [ "E", "A", "D", "G" ]
    , stringSelector = StringSelector.model
    , fretsSelector = FretsSelector.model
    , scaleSelector = ScaleSelector.init
    , selectedNotes = Set.fromList [ "E" ]
    }


view : Model -> Html.Html Action
view model =
    let
        highlightedNotes =
            let
                scale =
                    model.scaleSelector.selectedScale

                key =
                    model.scaleSelector.selectedKey

                indexes =
                    Dict.get scale Notes.scales

                baseIndex =
                    Notes.noteIndex key
            in
                case indexes of
                    Just indexes ->
                        Just <| List.map ((\a -> a + baseIndex)) indexes

                    Nothing ->
                        Nothing
    in
        div [ style [ ( "font-family", "monospace" ) ] ]
            [ Html.map Selector <| StringSelector.view model.stringSelector
            , Html.map FretsSelector <| FretsSelector.view model.fretsSelector
            , viewFretAnnotations model
            , viewFretSeparator model
            , div []
                (List.reverse <| List.indexedMap (viewString model) <| Array.toList model.strings)
            , Html.map ScaleSelector <| ScaleSelector.view model.scaleSelector
            , viewSelectedNotes model
            , viewMatchingScales model
            ]


viewSelectedNotes : Model -> Html.Html Action
viewSelectedNotes model =
    let
        viewNote n =
            div [ onClick (RemoveNote n) ] [ text n ]
    in
        div [] (List.map viewNote <| Set.toList model.selectedNotes)


viewMatchingScales : Model -> Html.Html Action
viewMatchingScales model =
    let
        folder : String -> Dict ( String, String ) Int -> Dict ( String, String ) Int
        folder note scores =
            let
                noteIx =
                    Notes.noteIndex note

                scalesForKey key =
                    List.map (\a -> ( a, key )) <| Maybe.withDefault [] <| Dict.get ((12 - Notes.noteIndex key + noteIx) % 12) Notes.scalesByOffset

                scalesWithKeys =
                    List.concat <| List.map scalesForKey (Array.toList Notes.notes)

                updater value =
                    case value of
                        Just v ->
                            Just <| v + 1

                        Nothing ->
                            Just 1

                addScores scale scores =
                    Dict.update scale updater scores
            in
                List.foldl addScores scores scalesWithKeys

        candidates =
            Set.toList model.selectedNotes
                |> List.foldl folder Dict.empty
                |> Dict.toList
                |> List.filter (\( ( scale, _ ), _ ) -> scale /= "Chromatic")
                |> List.map (\( a, score ) -> ( a, 100 * toFloat score / toFloat (Set.size model.selectedNotes) ))
                |> List.sortBy (\( _, score ) -> -score)

        viewCandidate ( ( scale, key ), score ) =
            li [] [ text <| toString <| round score, text "% = ", text scale, text " in ", text key ]

        indexes =
            List.map (\note -> ( note, Notes.noteIndex note )) <| Array.toList Notes.notes
    in
        ul [] (List.map viewCandidate candidates)


type CellType
    = Wider
    | Normal


cellStyle : CellType -> List ( String, String )
cellStyle ix =
    case ix of
        Wider ->
            [ ( "width", "80px" ), ( "text-align", "center" ) ]

        Normal ->
            [ ( "width", "40px" ), ( "text-align", "center" ) ]


viewString : Model -> Int -> String -> Html.Html Action
viewString model ix string =
    let
        startIndex =
            Notes.noteIndex string

        notesList =
            List.map (Notes.getNote << (\a -> a + startIndex))
                (List.range model.fretsSelector.startFret model.fretsSelector.endFret)

        highlightedNotes =
            let
                scale =
                    model.scaleSelector.selectedScale

                key =
                    model.scaleSelector.selectedKey

                indexes =
                    Dict.get scale Notes.scales

                baseIndex =
                    Notes.noteIndex key
            in
                case indexes of
                    Just indexes ->
                        Just <| List.map (Notes.getNote << (\a -> a + baseIndex)) indexes

                    Nothing ->
                        Nothing

        closeStyle =
            [ ( "cursor", "pointer" ), ( "margin-left", "5px" ) ]

        stringDescriptor =
            div [ style (cellStyle Wider) ]
                [ select [ onInput (ModifyString ix) ] stringCandidates
                , a [ style closeStyle, onClick (RemoveString ix) ] [ text "✖" ]
                ]

        renderCandidate note =
            option [ selected (string == note) ] [ text note ]

        stringCandidates =
            List.map renderCandidate <| Array.toList Notes.notes

        viewNote note =
            let
                isSelected =
                    case highlightedNotes of
                        Nothing ->
                            True

                        Just hl ->
                            List.member note hl
            in
                div
                    [ style
                        (List.append (cellStyle Normal)
                            [ ( "color"
                              , if isSelected then
                                    "black"
                                else
                                    "#ccc"
                              )
                            ]
                        )
                    , onClick (SelectNote note)
                    ]
                    [ text note ]
    in
        div [ style [ ( "display", "flex" ) ] ] (stringDescriptor :: List.map viewNote notesList)


viewFretAnnotations : Model -> Html.Html Action
viewFretAnnotations model =
    let
        annotation cellType ix =
            div [ style (cellStyle cellType) ] [ text <| toString ix ]

        frets =
            List.map (annotation Normal) (List.range model.fretsSelector.startFret model.fretsSelector.endFret)
    in
        div [ style [ ( "display", "flex" ) ] ] (annotation Wider 0 :: frets)


viewFretSeparator : Model -> Html.Html Action
viewFretSeparator model =
    let
        annotation cellType ix =
            div [ style (cellStyle cellType) ] [ text "--" ]

        frets =
            List.map (annotation Normal) (List.range model.fretsSelector.startFret model.fretsSelector.endFret)
    in
        div [ style [ ( "display", "flex" ) ] ] (annotation Wider 0 :: frets)


type Action
    = Selector StringSelector.Action
    | FretsSelector FretsSelector.Action
    | ScaleSelector ScaleSelector.Action
    | ModifyString Int String
    | RemoveString Int
    | SelectNote String
    | RemoveNote String


update action model =
    case action of
        Selector action ->
            let
                ( stringSelectorModel, newString ) =
                    StringSelector.update action model.stringSelector

                nextModel =
                    case newString of
                        Just string ->
                            { model
                                | strings = Array.push string model.strings
                                , stringSelector = stringSelectorModel
                            }

                        Nothing ->
                            { model | stringSelector = stringSelectorModel }
            in
                nextModel

        FretsSelector action ->
            { model | fretsSelector = FretsSelector.update action model.fretsSelector }

        ScaleSelector action ->
            { model | scaleSelector = ScaleSelector.update action model.scaleSelector }

        ModifyString ix str ->
            { model | strings = Array.set ix str model.strings }

        RemoveString ix ->
            let
                newStrings =
                    Array.append (Array.slice 0 ix model.strings)
                        (Array.slice (ix + 1) (Array.length model.strings) model.strings)
            in
                { model | strings = newStrings }

        SelectNote n ->
            { model | selectedNotes = Set.insert n model.selectedNotes }

        RemoveNote n ->
            { model | selectedNotes = Set.remove n model.selectedNotes }
