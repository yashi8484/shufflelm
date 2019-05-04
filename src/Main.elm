module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValue)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (setAt)
import Random exposing (generate)
import Random.List exposing (shuffle)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { inputValues : List String
    , resultValues : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputValues = []
      , resultValues = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Add
    | Change Int String
    | Shuffle
    | SetResult (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( { model | inputValues = model.inputValues ++ [ "" ] }
            , Cmd.none
            )

        Change index value ->
            let
                newInputValues =
                    List.Extra.setAt index value model.inputValues
            in
            ( { model | inputValues = newInputValues }
            , Cmd.none
            )

        Shuffle ->
            ( model
            , Random.generate SetResult (Random.List.shuffle model.inputValues)
            )

        SetResult result ->
            ( { model | resultValues = result }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.indexedMap viewInput model.inputValues)
        , ul [] (List.map viewValue model.inputValues)
        , button [ onClick Add ] [ text "+" ]
        , button [ onClick Shuffle ] [ text "shuffle" ]
        , ul [] (List.map viewValue model.resultValues)
        ]


viewInput : Int -> String -> Html Msg
viewInput i val =
    input [ placeholder "please input string.", value val, onInput (Change i) ] []


viewValue : String -> Html Msg
viewValue str =
    li [] [ text str ]
