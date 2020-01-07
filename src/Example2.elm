module Example2 exposing (main)

import Browser exposing (..)
import Element exposing (..)
import Element.Input as Input
import Framework.FormField exposing (..)
import Framework.Typography exposing (..)
import Framework.Modifier exposing (Modifier(..))
import Framework exposing (..)
import String exposing (toFloat,all)
import Char exposing (isDigit)
import Debug exposing (toString,log)


--Functions
celToFar : Float -> Float
celToFar val = (val * (9/5)) +32

farToCel : Float -> Float
farToCel val = (val-32)*(5/9)

--Types
type Msg = Change1 String
    | Change2 String 
--Model 
init =  
    { text1 = "" 
    , text2 = ""
    }
--View 
view model =layout [] <|
                row []
                    [ el [] <| Input.text []
                    { label = Input.labelBelow [] <| text "                                  "
                    , onChange = Change1
                    , placeholder = Nothing
                    , text = model.text1
                    }
                    ,
                    textSmall [] <| text " Celsius = "
                    ,
                    el [] <| Input.text []
                    { label = Input.labelBelow [] <| text "                                   "
                    , onChange = Change2
                    , placeholder = Nothing
                    , text = model.text2
                    },
                    textSmall [] <| text " Fahrenheit"
                    ]
--Update 
update msg model =
    case msg of
        Change1 newContent ->
            if  all isDigit newContent  then 
                { model | text1 = newContent, text2 = toString (celToFar(Maybe.withDefault 0 (String.toFloat newContent)))}
            else
                model
                
        Change2 newContent -> 
            if all isDigit newContent then 
                { model | text2 = newContent, text1 = toString (farToCel(Maybe.withDefault 0 (String.toFloat newContent)))}
            else 
                model
--Main 
main = Browser.sandbox 
    { init = init 
    , view = view 
    , update = update 
    }