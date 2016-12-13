module Notes exposing (notes, scales, noteIndex, getNote, scalesByOffset)

import Array
import Dict exposing (Dict)
import Dict


notes =
    Array.fromList [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


getNote : Int -> String
getNote ix =
    Maybe.withDefault "ERROR" <| Array.get (ix % (Array.length notes)) notes


noteIndex : String -> Int
noteIndex note =
    let
        list =
            Array.toIndexedList notes

        f ( ix, elem ) accum =
            case accum of
                Just ix ->
                    Just ix

                Nothing ->
                    if note == elem then
                        Just ix
                    else
                        Nothing
    in
        Maybe.withDefault (-1) (List.foldl f Nothing list)


scaleOffsetsRaw =
    [ ( "Major", [ 2, 2, 1, 2, 2, 2 ] )
    , ( "Chromatic", [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )
    , ( "Minor", [ 2, 1, 2, 2, 1, 2 ] )
    , ( "Major pentatonic", [ 2, 2, 3, 2 ] )
    , ( "Lydian", [ 2, 2, 2, 1, 2, 2 ] )
    , ( "Dorian", [ 2, 1, 2, 2, 3 ] )
    , ( "Mixolydian", [ 2, 2, 1, 2, 2, 1 ] )
    ]
        |> List.map (\( a, xs ) -> ( a, 0 :: xs ))


scales : Dict String (List Int)
scales =
    let
        mapper _ xs =
            List.foldl folder ( 0, [] ) xs |> (\( _, l ) -> l)

        folder elem ( accu, xs ) =
            ( accu + elem, List.append xs [ accu + elem ] )
    in
        Dict.fromList scaleOffsetsRaw
            |> Dict.map mapper


scalesByOffset : Dict Int (List String)
scalesByOffset =
    let
        recordNote : String -> Int -> Dict Int (List String) -> Dict Int (List String)
        recordNote scale note dict =
            let
                recorder newValue existing =
                    case existing of
                        Just x ->
                            Just <| newValue :: x

                        Nothing ->
                            Just [ newValue ]
            in
                Dict.update note (recorder scale) dict

        folder : String -> List Int -> Dict Int (List String) -> Dict Int (List String)
        folder scale notes dict =
            List.foldl (recordNote scale) dict notes
    in
        Dict.foldl folder Dict.empty scales
