module Example5 exposing (..)

import Browser
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Badge as Badge
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select 
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Utilities.Border as Border
import String exposing (startsWith,fromInt)
import List exposing (filter,map,head)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Html exposing (text)


--some function
toItem : User -> Select.Item msg
toItem {index,name,surname} = Select.item [ value (fromInt index)] [ text (surname++" , "++name)]


startsWith1 : String -> User -> Bool
startsWith1 match {name,surname} = (startsWith match name) || (startsWith match surname) || (match == "")
    
    
notSelected : String -> User -> Bool
notSelected selected {index} = selected /= fromInt(index)

updateUser : String -> String -> String -> User -> User
updateUser index1 name1 surname1 {index,name,surname} =
                if index1 == fromInt(index) then
                    User (Maybe.withDefault -1 (String.toInt(index1))) name1 surname1
                else 
                    User index name surname

isItSelected : String -> User -> Bool
isItSelected index1 {index} = (index1 == fromInt(index))

--Type Alias
type alias User =
  { index : Int
  , name : String
  , surname : String 
  }
--Msg
type Msg 
    = FilterInput String 
    | NameInput String
    | SurnameInput String 
    | ClickCreate 
    | ClickUpdate
    | ClickDelete
    | ChangeSelect String
--view 
view model = Grid.container [][
                CDN.stylesheet,
                Form.form[][
                    Form.formInline []
                    [ Form.label [] [ text "Filter Prefix:" ]
                        , Input.text [Input.attrs[], Input.onInput FilterInput] 
                    ],
                    Grid.row[]
                    [
                        Grid.col[Col.sm4][
                            Form.label [] [ text ""],
                            Select.select
                                [Select.attrs [multiple True], Select.onChange ChangeSelect ]
                                (List.map toItem (filter (startsWith1 model.match) (model.userList)))
                                
                                
                        ],
                        Grid.col[Col.sm3][Form.formInline []
                            [ Form.label [] [ text "Name:" ]
                                , Input.text [ Input.attrs [], Input.onInput NameInput, Input.value model.currentName],
                                Form.label [] [ text "Surname:" ]
                                , Input.text [ Input.attrs [], Input.onInput SurnameInput, Input.value model.currentSurname] 
                            ]
                        ]
                    ],
                    Grid.row[][
                        Grid.col[][
                            Button.button [Button.outlineDark, Button.onClick ClickCreate][text "Create"]
                        ],
                        Grid.col[][
                            Button.button [Button.outlineDark, Button.onClick ClickUpdate][text "Update"]
                        ],
                        Grid.col[][
                            Button.button [Button.outlineDark, Button.onClick ClickDelete][text "Delete"]
                        ],
                        Grid.col[Col.md8][
                            
                        ],
                        Grid.col [Col.md8][Form.label [] [ text "" ]]
                        
                    ]
                ]
            ]
--model
init = 
    { userList = 
                [ User 0 "Hanscc" "Emil"
                , User 1 "Max" "Mustermann"
                , User 2 "Roman" "Tisch"
                , User 3 "Adrian" "Ibarra"
                , User 4 "Jorge" "Perez"],
    match = "",
    currentName = "",
    currentSurname = "",
    createIndex = 5,
    selectedIndex = "-1"
    }
--update 
update msg model = 
    case msg of 
        FilterInput newFilter -> {model | match = newFilter}
        NameInput newName -> {model | currentName = newName}
        SurnameInput newSurname -> {model | currentSurname = newSurname }
        ChangeSelect newSelection -> 
                        let
                            userSelected = Maybe.withDefault (User 0 "" "")(head (filter(isItSelected newSelection) model.userList))
                        in
                            {model | selectedIndex = newSelection, currentName = userSelected.name, currentSurname = userSelected.surname }
        ClickDelete -> if model.selectedIndex == "-1" then 
                          model 
                        else 
                            
                                {model | 
                                userList = filter (notSelected model.selectedIndex) model.userList,selectedIndex = fromInt(Maybe.withDefault 0  (String.toInt(model.selectedIndex))+1),currentName = "", currentSurname = "" }

        ClickCreate ->{ model | userList = User model.createIndex model.currentName model.currentSurname :: model.userList,createIndex=model.createIndex+1} 
        ClickUpdate -> if model.selectedIndex == "-1" then 
                            model 
                        else 
                            {model| userList = (List.map (updateUser model.selectedIndex model.currentName model.currentSurname) model.userList)}
    
--main
main = Browser.sandbox 
    { view = view 
    , init = init 
    , update = update 
    }