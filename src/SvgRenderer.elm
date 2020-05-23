module SvgRenderer exposing (..)

import Html exposing (Html)
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


renderGameBoard game =
    svg
        [ width "800"
        , height "600"
        , viewBox "0 0 1000 1000"
        ]
        [ renderCard
            { suit = Clubs
            , rank = Ace
            , isFacedUp = True
            }
            { x = 10, y = 10 }
        ]


type alias Location =
    { x : Int
    , y : Int
    }


renderCard : Card -> Location -> Svg Msg
renderCard card location =
    Svg.image
        [ x (String.fromInt location.x)
        , y (String.fromInt location.y)

        --, width "50"
        , height "200"
        , xlinkHref (cardImgUrl card)
        ]
        []


renderCardStack : List Card -> Location -> Html Msg
renderCardStack cards location =
    Html.text "Hello card stack"



--renderCard : Card -> Html msg
--renderCard card =
--    let
--        rank =
--            rankString card.rank
--
--        suit =
--            suitString card.suit
--
--        hidden : List (Html.Attribute msg)
--        hidden =
--            if card.isFacedUp then
--                []
--
--            else
--                [ Html.Attributes.class "hiddenCard" ]
--    in
--    p hidden [ text (suit ++ rank) ]
--renderGameSlots : String -> List (List Card) -> Html msg
--renderGameSlots label gameSlots =
--    let
--        renderGameSlot : List Card -> Html msg
--        renderGameSlot cards =
--            cards
--                |> List.map (\card -> p [] [ renderCard card ])
--                |> div []
--
--        gameSlotDivs : List (Html msg)
--        gameSlotDivs =
--            gameSlots
--                |> List.indexedMap Tuple.pair
--                |> List.map
--                    (\( i, cards ) ->
--                        div
--                            []
--                            [ text ("stack " ++ String.fromInt i)
--                            , renderGameSlot cards
--                            ]
--                    )
--    in
--    div []
--        [ h2 [] [ text label ]
--        , div
--            [ Html.Attributes.class "gameStacks" ]
--            gameSlotDivs
--        ]
