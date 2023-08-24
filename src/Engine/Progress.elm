module Engine.Progress exposing (Progress, isDone, isNotDone, new, tick, toString)


type alias Progress =
    { current : Float, max : Float }


new : Float -> Progress
new max =
    Progress max max


tick : Float -> Progress -> Progress
tick dt progress =
    { progress | current = progress.current - dt |> max 0 }


toString : Progress -> String
toString { current, max } =
    String.fromFloat current ++ "/" ++ String.fromFloat max


isDone : Progress -> Bool
isDone { current } =
    current == 0


isNotDone : Progress -> Bool
isNotDone =
    isDone >> not
