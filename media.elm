--Olá
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String


-- model


type alias Model =
    { resultado : Float
    , notas : List Float
    , nota : Float
    , qtd : Float
    , situacao : String
    , error : Maybe String
    }


initModel : Model
initModel =
    { resultado = -1
    , notas = []
    , nota = -1
    , qtd = 0
    , situacao = ""
    , error = Nothing
    }



-- update


type Msg
    = Nota String
    | Adicionar
    | Calcular
    | Situacao
    | Limpar


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nota nota ->
            case String.toFloat nota of
                Ok input ->
                    { model
                        | nota = input
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | nota = -1
                        , error = Just err
                    }

        Adicionar ->
            let
                newNotas =
                    model.nota :: model.notas
            in
                { model
                    | notas = newNotas
                    , nota = -1
                    , qtd = model.qtd + 1
                }

        Calcular ->
            { model
                | resultado = (List.sum model.notas) / model.qtd
                , notas = []
                , nota = -1
                , qtd = 0
            }

        Situacao ->
            if model.resultado >= 5 then
                { model
                    | situacao = "Aprovado"
                }
            else
                { model
                    | situacao = "Reprovado"
                }

        Limpar ->
            initModel


view : Model -> Html Msg
view model =
    div []
        [ cabecalho model
        , div []
            [ text "Insira uma nota: "
            , input
                [ type' "text"
                , onInput Nota
                , value
                    (if model.nota == -1 then
                        ""
                     else
                        toString model.nota
                    )
                ]
                []
            , button
                [ type' "button"
                , onFocus Adicionar
                ]
                [ text "Adicionar" ]
            ]
        , div [] [ h3 [] [] ]
        , div []
            [ button
                [ type' "button"
                , onFocus Calcular
                , onClick Situacao
                ]
                [ text "Calcular" ]
            , button
                [ type' "button"
                , onClick Limpar
                ]
                [ text "Limpar" ]
            ]
        , div [] [ h3 [] [] ]
        , div []
            [ text
                ("Média final: "
                    ++ (if model.resultado == -1 then
                            ""
                        else
                            (toString model.resultado)
                       )
                )
            ]
        , div []
            [ text ("Situacao: " ++ model.situacao) ]
        ]


cabecalho : Model -> Html Msg
cabecalho model =
    div []
        [ h3 [] [ text "Programa de calculo de média e situação do aluno!" ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
