module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String


-- model


type alias Model =
    { resultado : Float
    , valor1 : Float
    , valor2 : Float
    , error : Maybe String
    }


initModel : Model
initModel =
    { resultado = 0
    , valor1 = 0
    , valor2 = 0
    , error = Nothing
    }



-- update


type Msg
    = Adicao
    | Subtracao
    | Multiplicacao
    | Divisao
    | Valor1 String
    | Valor2 String
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Adicao ->
            { model
                | resultado =
                    model.valor1 + model.valor2
                    --, valor1 = 0
                    --, valor2 = 0
            }

        Subtracao ->
            { model
                | resultado =
                    model.valor1 - model.valor2
                    --, valor1 = 0
                    --, valor2 = 0
            }

        Multiplicacao ->
            { model
                | resultado =
                    model.valor1 * model.valor2
                    --, valor1 = 0
                    --, valor2 = 0
            }

        Divisao ->
            { model
                | resultado =
                    model.valor1 / model.valor2
                    --, valor1 = 0
                    --, valor2 = 0
            }

        Valor1 val ->
            case String.toFloat val of
                Ok input ->
                    { model
                        | valor1 = input
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | valor1 = 0
                        , error = Just err
                    }

        Valor2 val ->
            case String.toFloat val of
                Ok input ->
                    { model
                        | valor2 = input
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | valor2 = 0
                        , error = Just err
                    }

        Clear ->
            initModel



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text ("Resultado: " ++ (toString model.resultado)) ]
        , div []
            [ text "Valor 1: "
            , input
                [ type' "text"
                , onInput Valor1
                , value
                    (if model.valor1 == 0 then
                        ""
                     else
                        toString model.valor1
                    )
                ]
                []
            ]
        , div []
            [ text "Valor 2: "
            , input
                [ type' "text"
                , onInput Valor2
                , value
                    (if model.valor2 == 0 then
                        ""
                     else
                        toString model.valor2
                    )
                ]
                []
            ]
        , div []
            []
        , div [] [ text (Maybe.withDefault "" model.error) ]
        , button
            [ type' "button"
            , onClick Adicao
            ]
            [ text "Adição" ]
        , button
            [ type' "button"
            , onClick Subtracao
            ]
            [ text "Subtração" ]
        , button
            [ type' "button"
            , onClick Multiplicacao
            ]
            [ text "Multiplicação" ]
        , button
            [ type' "button"
            , onClick Divisao
            ]
            [ text "Divisão" ]
        , button
            [ type' "button"
            , onClick Clear
            ]
            [ text "Clear" ]
        , p [] [ text (toString model) ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
