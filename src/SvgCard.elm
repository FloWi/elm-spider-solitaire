module SvgCard exposing (..)

import Html
import Messages exposing (Msg)
import Model exposing (Card, Rank(..), Suit(..))
import Svg exposing (Attribute, Svg, g, rect, svg, use)
import Svg.Attributes exposing (..)
import SvgSymbols exposing (rankId, rankSvgSymbol, suitId, suitSvgSymbol)


border =
    rect
        [ width "239"
        , height "335"
        , x "-119.5"
        , y "-167.5"
        , rx "12"
        , ry "12"
        , fill "white"
        , stroke "black"
        ]
        []


cardSvg : List (Html.Attribute Msg) -> Card -> Svg Msg
cardSvg attributes card =
    let
        rankSymbol =
            rankSvgSymbol card.rank card.suit

        suitSymbol =
            suitSvgSymbol card.suit

        topLeftRank =
            use
                [ xlinkHref ("#" ++ rankId card.rank card.suit)
                , height "32"
                , x "-114.4"
                , y "-156"
                ]
                []

        topLeftSuit =
            use
                [ xlinkHref ("#" ++ suitId card.suit)
                , height "26.769"
                , x "-111.784"
                , y "-119"
                ]
                []

        suits =
            use
                [ xlinkHref ("#" ++ suitId card.suit)
                , height "70"
                , x "-35"
                , y "-135.501"
                ]
                []

        group =
            g []
                [ topLeftRank
                , topLeftSuit
                , suits
                ]
    in
    svg
        ([ class "card"
         , height "3.5in"
         , width "2.5in"
         , preserveAspectRatio "none"
         , viewBox "-120 -168 240 336"
         ]
            ++ attributes
        )
        [ border
        , rankSymbol
        , suitSymbol
        , group
        , g [ transform "rotate(180)" ] [ group ]
        ]
