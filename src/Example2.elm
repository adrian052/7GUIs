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
celsiusToFahrenheith : Float -> Float
celsiusToFahrenheith val = (val * (9/5)) +32

fahrenheithToCelsius : Float -> Float
fahrenheithToCelsius val = (val-32)*(5/9)

--Types
type Msg = CelsiusToFahrenheith String
    | FahrenheithToCelsius String 
--Model 
init =  
    { celsius = "" 
    , fahrenheith = ""
    }
--View 
view model =layout [] <|
                row []
                    [ el [] <| Input.text []
                    { label = Input.labelBelow [] <| text "                                  "
                    , onChange = CelsiusToFahrenheith
                    , placeholder = Nothing
                    , text = model.celsius
                    }
                    ,
                    textSmall [] <| text " Celsius = "
                    ,
                    el [] <| Input.text []
                    { label = Input.labelBelow [] <| text "                                   "
                    , onChange = FahrenheithToCelsius
                    , placeholder = Nothing
                    , text = model.fahrenheith
                    },
                    textSmall [] <| text " Fahrenheit"
                    ]
--Update 
update msg model =
    case msg of
        CelsiusToFahrenheith newContent ->
            if  all isDigit newContent  then 
                { model | celsius = newContent, fahrenheith = toString (celsiusToFahrenheith(Maybe.withDefault 0 (String.toFloat newContent)))}
            else
                model
                
        FahrenheithToCelsius newContent -> 
            if all isDigit newContent then 
                { model | fahrenheith = newContent, celsius = toString (fahrenheithToCelsius(Maybe.withDefault 0 (String.toFloat newContent)))}
            else 
                model
--Main 
main = Browser.sandbox 
    { init = init 
    , view = view 
    , update = update 
    }