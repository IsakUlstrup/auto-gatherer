module Color exposing (Color, average, new, toString)


type alias Color =
    { hue : Float
    , saturation : Float
    , level : Float
    }


new : Float -> Float -> Float -> Color
new hue saturation level =
    Color (clamp 0 360 hue) (clamp 0 100 saturation) (clamp 0 100 level)


toString : Color -> String
toString color =
    "hsl("
        ++ String.fromFloat color.hue
        ++ ", "
        ++ String.fromFloat color.saturation
        ++ "%, "
        ++ String.fromFloat color.level
        ++ "%)"


average : List Color -> Color
average colors =
    let
        avgHue : Color -> List Color -> Float
        avgHue c cs =
            List.foldl (\clr sum -> sum + clr.hue) c.hue cs / toFloat colorCount

        avgSaturation : Color -> List Color -> Float
        avgSaturation c cs =
            List.foldl (\clr sum -> sum + clr.saturation) c.saturation cs / toFloat colorCount

        avgLevel : Color -> List Color -> Float
        avgLevel c cs =
            List.foldl (\clr sum -> sum + clr.level) c.level cs / toFloat colorCount

        colorCount =
            List.length colors
    in
    case colors of
        [] ->
            Color 0 0 0

        [ c ] ->
            c

        c :: cs ->
            Color (avgHue c cs) (avgSaturation c cs) (avgLevel c cs)
