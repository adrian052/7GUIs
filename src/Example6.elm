module Example6 exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Decode as D
import Array exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Alert as Alert 
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Html.Attributes exposing (href)
import String exposing (..)
import Bootstrap.Utilities.Border as Border
import Bootstrap.Button as Button
import Html.Events exposing (..)

--Functions
toElement : Comment ->ListGroup.CustomItem Msg
toElement comment = ListGroup.button [ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart, Html.Events.onClick (CommentSelected comment) ]]
                            [ p [ Spacing.mb1 ] [ text comment.text ]
                        , small [] [ text ((fromInt(comment.comment_count))++" Comment(s)") ]]
        
errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus _ ->
            "Unknown error"
        BadBody errorMessage ->
            errorMessage
                
--some types
type alias Comment =
    { id : String
    , text : String 
    , children : List(String)
    , comment_count : Int
    }

type CommentNode =
    CommentNode
        { id : String
        , text : String 
        , children : List(CommentNode)
        , comment_count : Int
        }

--model
type alias Model = 
                { threadList: Array Comment
                , commentSelected: Maybe Comment
                , listThreadSelected: List Comment
                , err: String  }

init : () -> (Model, Cmd Msg)
init _ = ( Model Array.empty Nothing [] "",getAllThreads)
--view 
view model = 
        case model.commentSelected of
            Nothing -> 
                Grid.container [][
                    CDN.stylesheet,
                    Alert.simpleDark [] [ text "Thread It Js" ],
                    Grid.container [Border.left, Border.right][
                        ListGroup.custom
                            (Array.toList(Array.map toElement model.threadList))
                    ]
                ]
            Just comment ->
                Grid.container [][
                    CDN.stylesheet,
                    Alert.simpleDark [] [ text "Thread It Js" ],
                    text ((fromInt(List.length model.listThreadSelected))++" "++model.err)   
                ]
            
--update 
type Msg 
        = GotThread (Result Http.Error (Array Comment))
        | GotComment (Result Http.Error  Comment)
        | CommentSelected Comment

update msg model = 
    case msg of
        GotThread result ->
            case result of
                Ok commentList -> ({model | threadList = commentList },Cmd.none)
                Err _ ->  (model,Cmd.none)
        GotComment result ->
            case result of
                Ok comment -> 
                    let 
                        ids = comment.children
                        cmdList = List.map (getComment) ids
                    in
                        ({model | listThreadSelected = comment :: model.listThreadSelected},Cmd.batch cmdList)

                Err error ->  ({model| err = errorToString error},Cmd.none)
        CommentSelected comment-> 
                    let 
                        ids = comment.children
                        cmdList = List.map (getComment) ids
                    in 
                        ({model | commentSelected = Just comment}, Cmd.batch cmdList)
--suscriptions
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--Html 
--Gets

getAllThreads : Cmd Msg
getAllThreads = 
    Http.get 
    { url = "http://api.threaditjs.com/threads"
    , expect = Http.expectJson GotThread threadDecoder 
    }


getComment : String -> Cmd Msg
getComment id = 
    Http.get 
    { url = "http://api.threaditjs.com/comments/"++id
    , expect = Http.expectJson GotComment commentDecoder 
    }
--Decoders

commentDecoder: Decoder Comment
commentDecoder = (field "data" (index 0 (commentStructureDecoder)))

commentStructureDecoder: Decoder Comment
commentStructureDecoder =
    map4 Comment 
        (at ["id"] string)
        (at ["text"] string)
        (at ["children"] (list string))
        (at ["comment_count"] int)

threadDecoder: Decoder (Array Comment)
threadDecoder = (field "data" (array(commentStructureDecoder)))

--main
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
