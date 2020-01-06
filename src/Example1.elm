module Example1 exposing (main)

import Element exposing (..)
import Element.Background
import Framework.Button as Button
import Framework.Color.Extra
import Framework.FormField exposing (..)
import Framework.Modifier exposing (Modifier(..))
import Html
import Browser

--Mensajes
type Msg = ClickButton 
--Model 
init = {count = 0}
--View
view model =
    layout [] <|
        row []
            [ el [] <| text ("   " ++(String.fromInt model.count)++ "   ")
            , el [ centerX, centerY ] <| Button.button [ Medium , Muted, Outlined ] (Just ClickButton) "Count" 
            ]

--Update
update msg model = 
    case msg of
        ClickButton ->
            {model | count = model.count + 1}


--Main
main = Browser.sandbox 
    { init = init
    , view = view
    , update = update
    }

                