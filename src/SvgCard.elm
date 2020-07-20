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


type RowPos
    = RowTop
    | Row2EntriesCenteredTop
    | Row2EntriesCenteredTopSlightlyOff
    | Row4Entries2ndFromTop
    | RowCenter
    | RowCourtCardTop


type ColPos
    = ColLeft
    | ColCenter
    | ColRight
    | ColCourtCardLeft
    | ColCourtCardRight


type alias SuitPos =
    ( RowPos, ColPos )


type alias Coord =
    { x : Float
    , y : Float
    }


suitPosLocation : SuitPos -> Coord
suitPosLocation ( rowPos, colPos ) =
    let
        x =
            case colPos of
                ColLeft ->
                    -87.501

                ColCenter ->
                    -35

                ColRight ->
                    17.501

                ColCourtCardLeft ->
                    -91.768

                ColCourtCardRight ->
                    36.088

        y =
            case rowPos of
                RowTop ->
                    -135.501

                Row2EntriesCenteredTop ->
                    -85.25

                Row2EntriesCenteredTopSlightlyOff ->
                    -102

                Row4Entries2ndFromTop ->
                    -68.5

                RowCenter ->
                    -35

                RowCourtCardTop ->
                    -132.16
    in
    { x = x, y = y }


type alias SuitPositions x =
    { symmetric : List x
    , nonSymmetric : List x
    }


innerSuitPositions : Card -> SuitPositions SuitPos
innerSuitPositions card =
    case card.rank of
        R2 ->
            { symmetric = [ ( RowTop, ColCenter ) ]
            , nonSymmetric = []
            }

        R3 ->
            { symmetric = [ ( RowTop, ColCenter ) ]
            , nonSymmetric = [ ( RowCenter, ColCenter ) ]
            }

        R4 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( RowTop, ColRight )
                ]
            , nonSymmetric = []
            }

        R5 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( RowTop, ColRight )
                ]
            , nonSymmetric = [ ( RowCenter, ColCenter ) ]
            }

        R6 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( RowTop, ColRight )
                ]
            , nonSymmetric =
                [ ( RowCenter, ColLeft )
                , ( RowCenter, ColRight )
                ]
            }

        R7 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( RowTop, ColRight )
                ]
            , nonSymmetric =
                [ ( RowCenter, ColLeft )
                , ( RowCenter, ColRight )
                , ( Row2EntriesCenteredTop, ColCenter )
                ]
            }

        R8 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( RowTop, ColRight )
                , ( Row2EntriesCenteredTop, ColCenter )
                ]
            , nonSymmetric =
                [ ( RowCenter, ColLeft )
                , ( RowCenter, ColRight )
                ]
            }

        R9 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( Row4Entries2ndFromTop, ColLeft )
                , ( RowTop, ColRight )
                , ( Row4Entries2ndFromTop, ColRight )
                ]
            , nonSymmetric =
                [ ( RowCenter, ColCenter ) ]
            }

        R10 ->
            { symmetric =
                [ ( RowTop, ColLeft )
                , ( Row4Entries2ndFromTop, ColLeft )
                , ( RowTop, ColRight )
                , ( Row4Entries2ndFromTop, ColRight )
                , ( Row2EntriesCenteredTopSlightlyOff, ColCenter )
                ]
            , nonSymmetric = []
            }

        _ ->
            { symmetric =
                []
            , nonSymmetric = []
            }



--Ace ->
--            { symmetric = []
--            , nonSymmetric = [ ( RowCenter, ColCenter ) ]
--            }
--Jack ->
--    { symmetric =
--        [ ( RowCourtCardTop, ColCourtCardLeft )
--        ]
--    , nonSymmetric = []
--    }
--
--Queen ->
--    { symmetric =
--        [ ( RowCourtCardTop, ColCourtCardRight )
--        ]
--    , nonSymmetric = []
--    }
--
--King ->
--    { symmetric =
--        [ ( RowCourtCardTop, ColCourtCardLeft )
--        ]
--    , nonSymmetric = []
--    }


indexPositions =
    { indexRankLeft = { x = -114.4, y = -156 }
    , indexRankRight = { x = 82.4, y = -156 }
    , indexSuitLeft = { x = -85, y = -156 }
    , indexSuitRight = { x = 54.5, y = -156 }
    }


cardSvg : List (Html.Attribute Msg) -> Card -> Svg Msg
cardSvg attributes card =
    let
        rankSymbol =
            rankSvgSymbol card.rank card.suit

        suitSymbol =
            suitSvgSymbol card.suit

        isCourthouse rank =
            case rank of
                King ->
                    True

                Queen ->
                    True

                Jack ->
                    True

                Ace ->
                    True

                _ ->
                    False

        indexSymbols =
            [ useFn (rankId card.rank card.suit) 30 indexPositions.indexRankLeft
            , useFn (rankId card.rank card.suit) 30 indexPositions.indexRankRight
            , useFn (suitId card.suit) 30 indexPositions.indexSuitLeft
            , useFn (suitId card.suit) 30 indexPositions.indexSuitRight
            ]

        courtHouseSymbol =
            if isCourthouse card.rank then
                [ useFn (rankId card.rank card.suit) 164.8 { x = -82.8, y = -82.4 } ]

            else
                []

        suitPositions =
            innerSuitPositions card

        useFn : String -> Float -> Coord -> Svg msg
        useFn id h pos =
            use
                [ xlinkHref ("#" ++ id)
                , height (String.fromFloat h)
                , x (String.fromFloat pos.x)
                , y (String.fromFloat pos.y)
                ]
                []

        scaleY : Float -> List Coord -> List Coord
        scaleY factor coords =
            coords
                |> List.map (\c -> { c | y = c.y * factor })

        symmetric =
            suitPositions.symmetric |> List.map suitPosLocation |> scaleY 0.95 |> List.map (useFn (suitId card.suit) 70) |> List.append indexSymbols

        nonSymmetric =
            suitPositions.nonSymmetric |> List.map suitPosLocation |> List.map (useFn (suitId card.suit) 70) |> List.append courtHouseSymbol
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
        , g [] nonSymmetric
        , g [] symmetric
        , g [ transform "rotate(180)" ] symmetric
        ]
