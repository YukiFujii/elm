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
    { resultado = 0
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
    | Situacao


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

        Situacao ->
            { model
                | resultado = (List.sum model.notas) / model.qtd
            }


view : Model -> Html Msg
view model =
    div []
        [ div []
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
                , onClick Situacao
                , onFocus Adicionar
                ]
                [ text "Adicionar" ]
            ]
        , div []
            [ text ("Quantidade de notas inseridas: " ++ (toString model.resultado)) ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
