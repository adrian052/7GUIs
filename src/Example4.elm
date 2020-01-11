module Example4 exposing (main)

import Browser
import Html exposing (..)
import Task exposing (..)
import Time exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Progress as Progress
import Bootstrap.Button as Button
import Modules.Range as Range
import String exposing (fromInt,toInt)



-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
type alias Model = 
                { time : Int
                , progress : Float
                , slices : Int
                , textDuration : String}


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 0.0 15 "Duration (15 sec)"
  , Cmd.none
  )


--Function 
changeRange = 
    Just (\ x -> ChangeRange x)

-- UPDATE
type Msg
  = Tick Time.Posix
  | Reset  
  | ChangeRange String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
        if (model.progress < 100) then
            ( {model | time = model.time + 1
            , progress = (100/toFloat(model.slices))*(toFloat(model.time +1))}
            , Cmd.none
            )  
        else
            (model, Cmd.none)    
    Reset ->
        ({model| time = 0
        , progress = 0.0}
        , Cmd.none) 
    ChangeRange newValue -> 
        ({model | slices = Maybe.withDefault 0 (toInt(newValue))
        , time = 0
        , progress = 0.0
        , textDuration = "Duration ("++newValue++" sec)"}
        , Cmd.none)    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Time.every 1000 Tick


--view
view model = Grid.container []
                [ CDN.stylesheet,   
                    Grid.row [][
                        Grid.col[]
                        [
                            text "Elapsed Timer"
                        ],
                        Grid.col[]
                        [
                            Progress.progress [Progress.value model.progress]
                        ]
                    ] ,
                    Grid.row [][
                        Grid.col[]
                        [
                            text (fromInt(model.time))
                        ],
                        Grid.col[]
                        [
                            
                        ]
                    ] ,   
                    Grid.row [][
                        Grid.col[]
                        [
                            text model.textDuration
                        ],
                        Grid.col[]
                        [
                            Range.range
                                [ Range.min "1"
                                , Range.max "30"
                                , Range.step "1"
                                , Range.value (fromInt (model.slices))
                                , Range.onInput changeRange
                                ]
                        ]
                    ],
                    Grid.row[][
                        Grid.col[][
                            Button.button [ Button.secondary , (Button.onClick Reset) ] [ text "Reset" ]
                        ]
                    ]
                ]