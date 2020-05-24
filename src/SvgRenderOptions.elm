module SvgRenderOptions exposing (renderOptions)


playStackXOffset =
    25


cardHeight =
    cardWidth / cardAspectRatio


cardWidth =
    100


cardAspectRatio : Float
cardAspectRatio =
    224 / 313


renderOptions :
    { cardInStackVerticalOffset : Int
    , playStackXOffset : Float
    , playStackTopOffset : Int
    , cardHeight : Float
    , cardWidth : Float
    }
renderOptions =
    { cardInStackVerticalOffset = 20
    , playStackXOffset = playStackXOffset
    , playStackTopOffset = playStackXOffset
    , cardHeight = cardHeight
    , cardWidth = cardWidth
    }
