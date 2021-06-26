module Fps exposing
    ( Model, init, Msg, update, subscriptions
    , fps
    )

{-| This library allows you to easily compute minimum, maximum, and average FPS for your application.


# Setup

@docs Model, init, Msg, update, subscriptions


# View

@docs fps

-}

import Browser.Events


{-| -}
type Msg
    = Tick Float


{-| -}
type Model
    = Uninit Int
    | Init
        { dts : List Float -- Dts in seconds?
        , maxDts : Int
        , min : Float
        , max : Float
        }


{-| Initializes the model, takes an integer representing how many frames we consider for the average FPS.

Usually, 20 is a good number to perceive regular variations in FPS.

-}
init : Int -> Model
init maxDts =
    Uninit maxDts


{-| Update the state when a new frame time is received.
-}
update : Msg -> Model -> Model
update (Tick dtSeconds) model =
    let
        currentFps =
            1 / dtSeconds
    in
    case model of
        Uninit maxDts ->
            Init
                { maxDts = maxDts
                , dts = [ dtSeconds ]
                , min = currentFps
                , max = currentFps
                }

        Init initializedModel ->
            Init
                { initializedModel
                    | dts = List.take initializedModel.maxDts (dtSeconds :: initializedModel.dts)
                    , min = min initializedModel.min currentFps
                    , max = max initializedModel.max currentFps
                }


{-| Subscribes to animation frame to compute frame times.
-}
subscriptions : Sub Msg
subscriptions =
    Browser.Events.onAnimationFrameDelta (\dtMillis -> Tick (dtMillis / 1000))


{-| Get FPS information.

Returns Nothing if no frame times have been received yet.

-}
fps : Model -> Maybe { average : Float, min : Float, max : Float }
fps model_ =
    case model_ of
        Uninit _ ->
            Nothing

        Init model ->
            let
                averageTimePerFrame =
                    List.sum model.dts / (toFloat <| List.length model.dts)
            in
            Just
                { average = 1 / averageTimePerFrame
                , min = model.min
                , max = model.max
                }
