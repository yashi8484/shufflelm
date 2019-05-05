module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, ol, text)
import Html.Attributes exposing (class, placeholder, value)
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
    { inputItems : List String
    , resultItems : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputItems = [ "" ]
      , resultItems = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddItem
    | ChangeItemValue Int String
    | Shuffle
    | SetResult (List String)
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem ->
            ( { model | inputItems = model.inputItems ++ [ "" ] }
            , Cmd.none
            )

        ChangeItemValue index value ->
            let
                newInputItems =
                    List.Extra.setAt index value model.inputItems
            in
            ( { model | inputItems = newInputItems }
            , Cmd.none
            )

        Shuffle ->
            ( model
            , Random.generate SetResult (Random.List.shuffle model.inputItems)
            )

        SetResult result ->
            ( { model | resultItems = result }
            , Cmd.none
            )

        Reset ->
            ( { model | inputItems = [ "" ], resultItems = [] }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container mx-auto pt-32 text-lg" ]
        [ div [ class "flex justify-center" ]
            [ div [ class "w-64" ]
                [ div [ class "card-title" ] [ text "Items" ]
                , div
                    [ class "card h-full text-center" ]
                    (List.indexedMap viewInputItem model.inputItems)
                ]
            , div [ class "w-48 p-4 text-center" ]
                [ button
                    [ class "btn btn-blue w-full my-4"
                    , onClick AddItem
                    ]
                    [ text "add item" ]
                , button
                    [ class "btn btn-blue w-full my-4"
                    , onClick Shuffle
                    ]
                    [ text "shuffle" ]
                , button
                    [ class "btn btn-red w-full my-4"
                    , onClick Reset
                    ]
                    [ text "reset" ]
                ]
            , div [ class "w-64" ]
                [ div [ class "card-title" ] [ text "Result" ]
                , div
                    [ class "card h-full text-center" ]
                    (List.map viewResultItem model.resultItems)
                ]
            ]
        ]


viewInputItem : Int -> String -> Html Msg
viewInputItem i item =
    input
        [ placeholder "input string"
        , value item
        , onInput (ChangeItemValue i)
        , class "input-text focus-blue my-2"
        ]
        []


viewResultItem : String -> Html Msg
viewResultItem item =
    input
        [ class "input-text pointer-events-none my-2"
        , value item
        ]
        []
