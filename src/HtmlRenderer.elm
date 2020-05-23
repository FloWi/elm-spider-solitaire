module HtmlRenderer exposing (..)

import Html exposing (Html, div, h1, h2, p, text)
import Html.Attributes
import Messages exposing (..)
import Model exposing (..)


renderHtml : Model -> Html Msg
renderHtml model =
    case model of
        UninitializedGame ->
            div []
                [ h1 [] [ text "uninitialized" ] ]

        RunningGame game ->
            div []
                [ h1 [] [ text "Game is on" ]
                , renderGameSlots "Game Slots" game.gameSlots
                , renderGameSlots "Draw new card stacks" game.drawCardSlots
                , renderGameSlots "completed card slots" game.completedCardSlots
                ]


renderCard : Card -> Html msg
renderCard card =
    let
        rank =
            rankString card.rank

        suit =
            suitString card.suit

        hidden : List (Html.Attribute msg)
        hidden =
            if card.isFacedUp then
                []

            else
                [ Html.Attributes.class "hiddenCard" ]
    in
    p hidden [ text (suit ++ rank) ]


renderGameSlots : String -> List (List Card) -> Html msg
renderGameSlots label gameSlots =
    let
        renderGameSlot : List Card -> Html msg
        renderGameSlot cards =
            cards
                |> List.map (\card -> p [] [ renderCard card ])
                |> div []

        gameSlotDivs : List (Html msg)
        gameSlotDivs =
            gameSlots
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, cards ) ->
                        div
                            []
                            [ text ("stack " ++ String.fromInt i)
                            , renderGameSlot cards
                            ]
                    )
    in
    div []
        [ h2 [] [ text label ]
        , div
            [ Html.Attributes.class "gameStacks" ]
            gameSlotDivs
        ]
