module Example3 exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html,select,option)
import Html.Attributes exposing (value)
import Element exposing (..)
import Element.Input as Input
import Element.Border exposing (width)
import Debug exposing (toString)
import Framework.Typography exposing (..)
import Framework.Modifier exposing (Modifier(..))
import Framework.Button as Button
import String exposing (split,toInt)
import Array exposing (..)
import Bootstrap.Alert as Alert
import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid


--Bootstrap 
alert model = Alert.config
        |> Alert.info
        -- This enables the alert to be dismissable with a fade out animation
        |> Alert.dismissableWithAnimation AlertMsg
        |> Alert.children
            [ Html.text model.alert]
        |> Alert.view model.alertVisibility

--Msg 
type Msg = 
    ChangeDate1 String 
    | ChangeDate2 String 
    | ChangeType String
    | Book 
    | AlertMsg Alert.Visibility

--Validation 
validateDate: String -> Bool
validateDate date = 
        let 
            arrayDate = separateFormat date
            day = (Maybe.withDefault -1 (get 0 arrayDate))
            month = (Maybe.withDefault -1 (get 1 arrayDate)) 
            year = (Maybe.withDefault -1 (get 2 arrayDate))
        in 
            if length arrayDate == 3 then 
                if isNotDefault day && isNotDefault month && isNotDefault year then
                    isDate year month day
                else 
                    False
            else 
                False

separateFormat:  String -> Array (Int)
separateFormat date = Array.map (Maybe.withDefault -1)(Array.map (toInt)(fromList(split "." date)))

isNotDefault: Int -> Bool
isNotDefault value = value /= -1

isDate: Int -> Int -> Int -> Bool
isDate year month day =
    let
        monthDays = fromList [31, 28, 31, 30,31, 30, 31, 31, 30, 31, 30, 31]
    in
        if month < 1 || month > 12 then 
            False
        else if (day < 1 || day > Maybe.withDefault -1 (get (month-1) monthDays)) then 
            False
        else if year < 2000 || year > 3000 then 
            False
        else 
            True

compareDate: String -> String -> Int
compareDate date1 date2 =
    let 
        arrayDate1 = separateFormat date1
        arrayDate2 = separateFormat date2
    in
        if ( Maybe.withDefault  -1 (get (2) arrayDate1)) < Maybe.withDefault -1 (get (2) arrayDate2)  then
            -1
        else if ( Maybe.withDefault  -1 (get (2) arrayDate1)) > Maybe.withDefault  -1 (get (2) arrayDate2)  then
            1
        else 
            if ( Maybe.withDefault  -1 (get (1) arrayDate1) < Maybe.withDefault  -1 (get (1) arrayDate2) ) then
                -1
            else if ( Maybe.withDefault  -1 (get (1) arrayDate1) > Maybe.withDefault  -1 (get (1) arrayDate2) ) then
                1
            else 
                if ( Maybe.withDefault  -1 (get (0) arrayDate1) < Maybe.withDefault  -1 (get (0) arrayDate2) ) then
                    -1
                else if ( Maybe.withDefault  -1 (get (0) arrayDate1) > Maybe.withDefault  -1 (get (0) arrayDate2) ) then
                    1
                else
                    0
        
    

--View 
view model = 
        case model.shownMessage of 
        True ->
            Grid.container []
            [ CDN.stylesheet      
            , alert model 
            ]
        False -> 
            (Html.form []
                [
                layout [] <|
                    column [centerX]
                        [Input.radioRow []
                        { label = Input.labelAbove [] <| Element.text ""
                        , onChange = ChangeType
                        , selected = Just model.selectedType
                        , options =
                            [ Input.option "oneWayFlight" (Element.text "One-Way Flight")
                            , Input.option "returnFlight" (Element.text "Return Flight")
                            ]
                        } 
                        ,
                        textSmall [] <| Element.text " " ,
                        el [] <| Input.text []
                        { label = Input.labelBelow [] <| Element.text "           "
                        , onChange = ChangeDate1
                        , placeholder = Nothing
                        , text = model.date1
                        }
                        ,
                        el [] <| Input.text [alpha model.alphaDate2]
                        { label = Input.labelBelow [] <| Element.text "         "
                        , onChange = ChangeDate2
                        , placeholder = Nothing
                        , text = model.date2    
                        }
                        ,
                        el [centerX,alpha model.alphtBook] <| Button.button [Outlined , Primary] (Just Book) "Book"
                        ]
                ]
            )
--Model 
init = 
    { date1 = "07.01.2020"
    , date2 = "07.01.2020"
    , selectedType = "oneWayFlight"
    , alphaDate2 = 0.5
    , alphtBook = 1
    , alert = "You have booked a one-way flight for 07.01.2020"
    , alertVisibility = Alert.shown
    , shownMessage = False
    }

--Update 
update msg model = 
    case msg of 
        ChangeDate1 newDate -> 
            if validateDate newDate  then 
                if model.selectedType == "oneWayFlight" then 
                    {model | date1 = newDate, alphtBook = 1}
                else 
                    if  compareDate newDate model.date2 == 1 then 
                        {model | date1 = newDate, alphtBook = 0.3}
                    else  
                        {model | date1 = newDate, alphtBook = 1 }
            else    
                    {model | date1 = newDate, alphtBook = 0.3}
        ChangeDate2 newDate ->
            if model.selectedType == "returnFlight"  then
                if  validateDate newDate  then 
                    if (compareDate model.date1 newDate) < 1 then
                        {model | date2 = newDate  , alphtBook = 1}
                    else    
                        {model | date2 = newDate, alphtBook =0.3}
                else 
                    {model | alphtBook = 0.3, date2 = newDate}
            else 
                model
        ChangeType newSelection ->
            
            if newSelection == "returnFlight" then
                {model | selectedType = newSelection , alphaDate2 = 1} 
            else 
                {model | selectedType = newSelection , alphaDate2 = 0.5} 
        Book -> 
            if model.selectedType == "oneWayFlight"  then
                if validateDate model.date1 then 
                    {model | shownMessage = True, alert = "You have booked a one-way flight for "++model.date1 }
                else 
                    model 
            else 
                if validateDate model.date1 && validateDate model.date2 && (compareDate model.date1 model.date2) < 1 then 
                    {model | shownMessage = True , alert = "You have booked a return flight from "++model.date1++" to "++model.date2}
                else 
                    model
        AlertMsg alerta -> 
            {model | shownMessage = False}
        

--Main 
main = Browser.sandbox
    { init = init 
    , view = view 
    , update = update
    }
