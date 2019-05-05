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
    div [ class "container mx-auto pt-32 font-sans text-lg" ]
        [ div [ class "flex justify-center" ]
            [ div [ class "w-64" ]
                [ div [ class "font-bold text-xl mb-2" ] [ text "Items" ]
                , div [ class "p-2 text-center border border-grey-light shadow-lg h-full" ]
                    (List.indexedMap viewInputItem model.inputItems)
                ]
            , div [ class "w-48 text-center p-4" ]
                [ button
                    [ class "bg-blue shadow-lg text-white uppercase py-2 px-4 w-full rounded-full my-4"
                    , onClick AddItem
                    ]
                    [ text "add item" ]
                , button
                    [ class "bg-blue shadow-lg text-white uppercase py-2 px-4 w-full rounded-full my-4"
                    , onClick Shuffle
                    ]
                    [ text "shuffle" ]
                , button
                    [ class "bg-red shadow-lg text-white uppercase py-2 px-4 w-full rounded-full my-4"
                    , onClick Reset
                    ]
                    [ text "reset" ]
                ]
            , div [ class "w-64" ]
                [ div [ class "font-bold text-xl mb-2" ] [ text "Result" ]
                , div
                    [ class "p-2 text-center border border-grey-light shadow-lg h-full" ]
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
        , class "my-2 appearance-none bg-transparent border-b border-b-2 border-grey-light py-1 px-2 text-grey-darker leading-tight focus:outline-none focus:border-blue"
        ]
        []


viewResultItem : String -> Html Msg
viewResultItem item =
    input
        [ class "my-2 appearance-none bg-transparent border-b border-b-2 border-grey-light py-1 px-2 text-grey-darker leading-tight pointer-events-none"
        , value item
        ]
        []
