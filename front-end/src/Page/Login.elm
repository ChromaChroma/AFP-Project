module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser                           exposing (Document)
import Html                              exposing (..)
import Html.Attributes                   exposing (..)
import Html.Events                       exposing (..)
import Debug                             exposing (log)
import Json.Decode        as Decode      exposing (Decoder, decodeString, field, string)
import Task                              exposing (Task)
import Http
import Browser.Navigation as Nav
import Json.Encode        as Encode 
import Page.CodeProblem   as CodeProblem
import Route                             exposing (pushUrl)
import Error                             exposing (errorToStr)

-- TODO: fix routing, handled access correctly (Using session) & refreshing of token, show output of code, abstract Http with endpoints, update css styling, register page, choce codeproblem page

-- MODEL

type alias Model =
  { navKey : Nav.Key 
  , user   : User
  }

type alias User =
  { username : String
  , password : String
  }

type alias Cred =
    { access  : String
    , refresh : String   
    }

init : Nav.Key -> (Model, Cmd msg)
init navKey = 
    ({ navKey = navKey
     , user   =
            { username = ""
            , password = ""
            }
     }
    , Cmd.none
    )

-- UPDATE

type Msg
  = SetUsername    String
  | SetPassword    String
  | SubmitLogin
  | CompletedLogin (Result Http.Error Cred)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitLogin -> --(model, Cmd.msg GotCodeProblemMsg CodeProblem.Loading)
            let 
                -- request = submitRequest model.user
                request =  Route.pushUrl Route.CodeProblem model.navKey-- tmp just to test routing
            in 
            ( model
            , request
            )


        SetUsername username ->
            updateCred (\user -> { user | username = username }) model

        SetPassword password ->
            updateCred (\user -> { user | password = password }) model

        CompletedLogin (Ok cred) ->
            ( model
            , Cmd.none
            ) |> Debug.log "nicee login"

        CompletedLogin (Err errorMsg) -> (model, Cmd.none) |> Debug.log ("login error: " ++ (errorToStr errorMsg))


updateCred : (User -> User) -> Model -> ( Model, Cmd Msg )
updateCred transform model =
    ( { model | user = transform model.user }
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title   = "Login"
    , content =
        div [ class "cred-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
                        , viewUserLogin model.user
                        ]
                    ]
                ]
            ]
    }

viewUserLogin : User -> Html Msg
viewUserLogin form =
    Html.form [ onSubmit SubmitLogin ]
        [ fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , placeholder "username"
                , onInput SetUsername
                , value form.username
                ]
                []
            ]
        , fieldset [ class "form-group" ]
            [ input
                [ class "form-control form-control-lg"
                , type_ "password"
                , placeholder "Password"
                , onInput SetPassword
                , value form.password
                ]
                []
            ]
        , button [ class "btn btn-lg btn-primary pull-xs-right" ]
            [ text "Sign in" ]
        ]



-- HTTP


userEncoder : User -> Encode.Value
userEncoder user = 
    Encode.object 
        [("username", Encode.string user.username)
        ,("password", Encode.string user.password)
        ]

credDecoder : Decoder Cred
credDecoder =
  Decode.map2 Cred
    (Decode.field "access" Decode.string)
    (Decode.field "refresh" Decode.string)

-- userDecoder : Decoder User
-- userDecoder =
--   Decode.map2 User
--     (Decode.field "access" Decode.string)
--     (Decode.field "refresh" Decode.string)


submitRequest : User -> Cmd Msg
submitRequest user =
    -- Http.post
    --     { body = Http.jsonBody (userEncoder user)
    --      , expect = Http.expectJson CompletedLogin credDecoder
    --     , url = "http://localhost:8080/login"
    --     }
    let
        hs : List Http.Header
        hs =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]

        request =
            { method = "POST"
            , headers = hs
            , url = "http://localhost:8080/login"
            , body = Http.jsonBody (userEncoder user)
            , expect = Http.expectJson CompletedLogin credDecoder
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request request