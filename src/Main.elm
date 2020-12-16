port module Main exposing (main)

import Array
import Browser
import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Decode as Decode
import Lattice exposing (lattice)
import Note exposing (Note)
import NoteSet exposing (NoteSet)
import Process exposing (sleep)
import Random
import Svg.Attributes
import Svg.Events
import Task


port askJs : Array.Array { wait : Float, ratio : ( Int, Int ) } -> Cmd msg


demo : ( Int, Int ) -> Cmd msg
demo ratio =
    let
        r =
            if ratio == ( 1, 1 ) then
                ( 2, 1 )

            else
                ratio
    in
    askJs <|
        Array.fromList
            [ { wait = 0.5, ratio = ( 1, 1 ) }
            , { wait = 1, ratio = r }
            , { wait = 3, ratio = ( 1, 1 ) }
            , { wait = 0, ratio = r }
            , { wait = 1, ratio = r }
            , { wait = 1, ratio = ( 1, 1 ) }
            , { wait = 3, ratio = r }
            , { wait = 0, ratio = ( 1, 1 ) }
            , { wait = 1, ratio = ( 1, 1 ) }
            , { wait = 0, ratio = r }
            , { wait = 4, ratio = ( 1, 1 ) }
            , { wait = 0, ratio = r }
            ]


quiet : Cmd msg
quiet =
    askJs Array.empty



--------------------------------------------------------------------------------


type Model
    = Menu
        { seed : Random.Seed
        , selected : NoteSet
        }
    | Running
        { seed : Random.Seed
        , notes : NoteSet
        , answer : Note
        , status : Status
        , score : Score
        }


type alias Score =
    { correct : Int
    , total : Int
    }


type Status
    = Waiting
    | Correct
    | Incorrect Note


type Msg
    = Click Note
    | Begin
    | ReturnToMenu


main : Program Float Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Float -> ( Model, Cmd Msg )
init rand =
    ( Menu
        { seed = Random.initialSeed <| floor <| rand * 0x00100000
        , selected =
            List.foldl NoteSet.add
                NoteSet.new
                [ Note -1 0 0
                , Note 0 0 0
                , Note 1 0 0
                , Note 2 0 0
                , Note -1 1 0
                , Note 0 1 0
                , Note 1 1 0
                ]
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Menu { selected, seed } ->
            case msg of
                Click note ->
                    ( Menu
                        { seed = seed
                        , selected =
                            if NoteSet.member note maxNotes then
                                NoteSet.toggle note selected

                            else
                                selected
                        }
                    , Cmd.none
                    )

                Begin ->
                    if NoteSet.isEmpty selected then
                        ( model, Cmd.none )

                    else
                        quizPlayer seed selected (Score 0 0)

                _ ->
                    ( model, Cmd.none )

        Running ({ notes, seed, answer, status, score } as state) ->
            case msg of
                Click note ->
                    if NoteSet.member note notes then
                        case status of
                            Waiting ->
                                ( if note == answer then
                                    Running
                                        { state
                                            | status = Correct
                                            , score =
                                                { score
                                                    | correct = score.correct + 1
                                                    , total = score.total + 1
                                                }
                                        }

                                  else
                                    Running
                                        { state
                                            | status = Incorrect note
                                            , score =
                                                { score
                                                    | total = score.total + 1
                                                }
                                        }
                                , Cmd.batch
                                    [ quiet
                                    , Task.perform (\() -> Begin) (sleep 1000)
                                    ]
                                )

                            _ ->
                                ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Begin ->
                    case status of
                        Correct ->
                            quizPlayer seed notes score

                        _ ->
                            ( Running { state | status = Waiting }
                            , demo (Note.ratio answer)
                            )

                ReturnToMenu ->
                    ( Menu { seed = seed, selected = notes }, quiet )


quizPlayer : Random.Seed -> NoteSet -> Score -> ( Model, Cmd msg )
quizPlayer seed notes score =
    let
        noteList =
            NoteSet.fold (::) [] notes

        gen =
            case noteList of
                [] ->
                    Random.constant (Note 0 0 0)

                first :: rest ->
                    Random.uniform first rest

        ( answer, seed2 ) =
            Random.step gen seed
    in
    ( Running
        { seed = seed2
        , notes = notes
        , answer = answer
        , status = Waiting
        , score = score
        }
    , demo (Note.ratio answer)
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Ear Trainer"
    , body =
        [ Html.node "style" [] [ Html.text styles ]
        , layout [ behindContent (viewLattice model) ] <|
            column
                [ width fill
                , height fill
                , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                , Font.size 48
                ]
                [ el [ alignTop, centerX, padding 20 ] <|
                    instructions model
                , row
                    [ alignBottom
                    , centerX
                    , width shrink
                    , padding 20
                    , spacing 40
                    , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                    ]
                    (buttons model)
                ]
        ]
    }


viewLattice : Model -> Element Msg
viewLattice model =
    case model of
        Menu { selected } ->
            lattice maxNotes
                (\note ->
                    [ Svg.Attributes.class <|
                        if NoteSet.member note selected then
                            "selected"

                        else
                            "deselected"
                    , Svg.Events.onClick (Click note)
                    ]
                )

        Running { notes, status, answer } ->
            lattice notes
                (\note ->
                    [ case status of
                        Waiting ->
                            Svg.Attributes.class "selected"

                        Correct ->
                            if note == answer then
                                Svg.Attributes.fill "#080"

                            else
                                Svg.Attributes.fill "#aaa"

                        Incorrect guess ->
                            if note == guess then
                                Svg.Attributes.fill "#e00"

                            else
                                Svg.Attributes.fill "#aaa"
                    , Svg.Events.onClick (Click note)
                    ]
                )


instructions : Model -> Element msg
instructions model =
    case model of
        Menu _ ->
            text "Select intervals to train."

        Running _ ->
            textColumn [ Font.center ]
                [ text "Select the interval you hear."
                , el [ Font.size 24 ] <| text "(Treat octaves as unisons)"
                ]


buttons : Model -> List (Element Msg)
buttons model =
    case model of
        Menu { selected } ->
            [ if NoteSet.isEmpty selected then
                none

              else
                Input.button buttonAttributes
                    { onPress = Just Begin
                    , label = Element.text "Begin!"
                    }
            ]

        Running { score } ->
            [ Input.button buttonAttributes
                { onPress = Just ReturnToMenu
                , label = Element.text "Menu"
                }
            , Input.button buttonAttributes
                { onPress = Just Begin
                , label = Element.text "Repeat Notes"
                }
            , Element.text <| String.concat [ "Score: ", String.fromInt score.correct, "/", String.fromInt score.total ]
            ]


buttonAttributes : List (Element.Attribute Msg)
buttonAttributes =
    [ Element.htmlAttribute <| Html.Attributes.class "deselected"
    , padding 10
    , Border.width 5
    , Border.rounded 30
    ]


maxNotes : NoteSet
maxNotes =
    List.foldl NoteSet.add
        NoteSet.new
        [ Note 0 1 -1
        , Note 1 0 -1
        , Note 0 0 -1

        --
        , Note -1 -2 0
        , Note 0 -2 0
        , Note 1 -2 0
        , Note -3 -1 0
        , Note -2 -1 0
        , Note -1 -1 0
        , Note 0 -1 0
        , Note 1 -1 0
        , Note 2 -1 0
        , Note 3 -1 0
        , Note -4 0 0
        , Note -3 0 0
        , Note -2 0 0
        , Note -1 0 0
        , Note 0 0 0
        , Note 1 0 0
        , Note 2 0 0
        , Note 3 0 0
        , Note 4 0 0
        , Note -3 1 0
        , Note -2 1 0
        , Note -1 1 0
        , Note 0 1 0
        , Note 1 1 0
        , Note 2 1 0
        , Note 3 1 0
        , Note -1 2 0
        , Note 0 2 0
        , Note 1 2 0

        --
        , Note 0 -1 1
        , Note -1 0 1
        , Note 0 0 1
        , Note 1 0 1
        ]


styles : String
styles =
    """
.test { height: 100vh }
.deselected        { fill: #eee; background-color: #eee; }
.deselected:hover  { fill: #ccc; background-color: #ccc; }
.deselected:active { fill: #666; background-color: #666; }
.selected          { fill: #aaa; background-color: #aaa; }
.selected:hover    { fill: #888; background-color: #888; }
.selected:active   { fill: #666; background-color: #666; }
"""


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        (Decode.field "code" Decode.string
            |> Decode.andThen
                (\k ->
                    case Dict.get k keyMap of
                        Just msg ->
                            Decode.succeed msg

                        Nothing ->
                            Decode.fail ""
                )
        )


keyMap : Dict String Msg
keyMap =
    Dict.fromList
        [ ( "Digit1", Click <| Note -6 2 0 )
        , ( "Digit2", Click <| Note -5 2 0 )
        , ( "Digit3", Click <| Note -4 2 0 )
        , ( "Digit4", Click <| Note -3 2 0 )
        , ( "Digit5", Click <| Note -2 2 0 )
        , ( "Digit6", Click <| Note -1 2 0 )
        , ( "Digit7", Click <| Note 0 2 0 )
        , ( "Digit8", Click <| Note 1 2 0 )
        , ( "Digit9", Click <| Note 2 2 0 )
        , ( "Digit0", Click <| Note 3 2 0 )
        , ( "KeyQ", Click <| Note -5 1 0 )
        , ( "KeyW", Click <| Note -4 1 0 )
        , ( "KeyE", Click <| Note -3 1 0 )
        , ( "KeyR", Click <| Note -2 1 0 )
        , ( "KeyT", Click <| Note -1 1 0 )
        , ( "KeyY", Click <| Note 0 1 0 )
        , ( "KeyU", Click <| Note 1 1 0 )
        , ( "KeyI", Click <| Note 2 1 0 )
        , ( "KeyO", Click <| Note 3 1 0 )
        , ( "KeyP", Click <| Note 4 1 0 )
        , ( "KeyA", Click <| Note -4 0 0 )
        , ( "KeyS", Click <| Note -3 0 0 )
        , ( "KeyD", Click <| Note -2 0 0 )
        , ( "KeyF", Click <| Note -1 0 0 )
        , ( "KeyG", Click <| Note 0 0 0 )
        , ( "KeyH", Click <| Note 1 0 0 )
        , ( "KeyJ", Click <| Note 2 0 0 )
        , ( "KeyK", Click <| Note 3 0 0 )
        , ( "KeyL", Click <| Note 4 0 0 )
        , ( "KeyZ", Click <| Note -3 -1 0 )
        , ( "KeyX", Click <| Note -2 -1 0 )
        , ( "KeyC", Click <| Note -1 -1 0 )
        , ( "KeyV", Click <| Note 0 -1 0 )
        , ( "KeyB", Click <| Note 1 -1 0 )
        , ( "KeyN", Click <| Note 2 -1 0 )
        , ( "KeyM", Click <| Note 3 -1 0 )
        , ( "Space", Begin )
        ]
