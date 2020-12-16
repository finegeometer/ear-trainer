module Lattice exposing (lattice)

import Element
import List exposing (concat)
import Note exposing (Note)
import NoteSet exposing (NoteSet)
import Svg exposing (svg)
import Svg.Attributes exposing (..)


lattice : NoteSet -> (Note -> List (Svg.Attribute msg)) -> Element.Element msg
lattice notes attrs =
    Element.html <|
        svg
            [ viewBox "0, 0, 1400, 1000"
            , height "100%"
            , width "100%"
            ]
        <|
            NoteSet.fold
                (\note ->
                    (::) <|
                        Svg.g
                            [ transform
                                (String.concat
                                    [ "translate("
                                    , String.fromInt <| 700 + 200 * note.threes + 100 * note.fives + 100 * note.sevens
                                    , " "
                                    , String.fromInt <| 500 - 173 * note.fives - 58 * note.sevens
                                    , ") "
                                    ]
                                )
                            ]
                        <|
                            let
                                conditionalPrependLine cond x y list =
                                    if cond then
                                        Svg.line [ x1 "0", y1 "0", x2 x, y2 y, stroke "#000" ] [] :: list

                                    else
                                        list
                            in
                            [ Svg.g
                                [ transform
                                    (String.concat
                                        [ "scale("
                                        , String.fromFloat <| 2 / (2 + toFloat (abs note.sevens))
                                        , ") "
                                        ]
                                    )
                                ]
                                [ Svg.circle
                                    (r "50" :: stroke "black" :: strokeWidth "3" :: attrs note)
                                    []
                                , Svg.text_
                                    [ fill "#000"
                                    , textAnchor "middle"
                                    , dominantBaseline "middle"
                                    , fontSize "48"
                                    , Svg.Attributes.style "pointer-events: none;"
                                    ]
                                    [ Svg.text (Note.name note) ]
                                ]
                            ]
                                |> conditionalPrependLine (note.sevens == 0 && NoteSet.member { note | threes = note.threes + 1 } notes) "200" "0"
                                |> conditionalPrependLine (note.sevens == 0 && NoteSet.member { note | fives = note.fives + 1 } notes) "100" "-173"
                                |> conditionalPrependLine (note.sevens == 0 && NoteSet.member { note | threes = note.threes + 1, fives = note.fives - 1 } notes) "100" "173"
                                |> conditionalPrependLine (NoteSet.member { note | sevens = note.sevens + 1 } notes) "100" "-58"
                )
                []
                notes
