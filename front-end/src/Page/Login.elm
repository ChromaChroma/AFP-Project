module Page.Login exposing (Model, Msg, init, subscriptions, update, view, toSession)

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
import Utils.Error                             exposing (errorToStr)
import Session                           exposing ( getNavKey)
import Utils.Types exposing (..)
-- TODO: fix routing, handled access correctly (Using session) & refreshing of token, show output of code, abstract Http with endpoints, update css styling, register page, choce codeproblem page

-- MODEL

-- type alias Model =
--   { session : Session 
--   , user    : User
--   }

type alias Model = LoginModel

-- type alias Cred =
--     { access  : String
--     , refresh : String   
--     }

init : Session -> (Model, Cmd msg)
init session = 
    ({ session = session
     , user   =
            { username = ""
            , password = ""
            }
     }
    , Cmd.none
    )

-- UPDATE

type Msg
  = UpdateInput    Field String
  | SubmitLogin
  | CompletedLogin (Result Http.Error Cred)
  | GotSession Session



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitLogin -> --(model, Cmd.msg GotCodeProblemMsg CodeProblem.Loading)
            let 
                request = submitRequest model.user
                -- request =  Route.pushUrl Route.CodeProblem (getNavKey model.session) -- tmp just to test routing
            in 
            ( model
            , request
            )


        UpdateInput Username username ->
            updateCred (\user -> { user | username = username }) model

        UpdateInput Password password ->
            updateCred (\user -> { user | password = password }) model

        CompletedLogin (Ok cred) ->
            let
                updateModel = { model | session = Authenticated (getNavKey model.session) cred }
                request =  Route.pushUrl CodingProblemRoute (getNavKey model.session)
            in
            ( updateModel
            , Cmd.batch [request, Session.store cred] --request
            ) |> Debug.log "nicee login"

        CompletedLogin (Err errorMsg) -> (model, Cmd.none) |> Debug.log ("login error: " ++ (errorToStr errorMsg))

        GotSession session ->
            ( { model | session = session }
            , Route.pushUrl CodingProblemRoute (getNavKey model.session) |> Debug.log "ppppppppppp"
            )


updateCred : (User -> User) -> Model -> ( Model, Cmd Msg )
updateCred transform model =
    ( { model | user = transform model.user }
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Session.changes GotSession (getNavKey model.session) |> Debug.log "ooooooooooooooooooo"

-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "login-container" ]
            [ h2 [] [ text "Login" ]
            -- , viewForm model
            , Html.form [ class "login-form", onSubmit SubmitLogin ]
                [ label [ for "username" ] [ text "Username" ]
                , input [ type_ "text", id "username", name "username", placeholder "Enter your username", value model.user.username, onInput (UpdateInput Username) ] []
                , label [ for "password" ] [ text "Password" ]
                , input [ type_ "password", id "password", name "password", placeholder "Enter your password", value model.user.password, onInput (UpdateInput Password) ] []
                , button [ type_ "submit" ] [ text "Login" ]
                ]
            ]
        }

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
    let
        request =
            { method = "POST"
            , headers = []
            , url = "http://localhost:8080/login"
            , body = Http.jsonBody (userEncoder user)
            , expect = Http.expectJson CompletedLogin credDecoder
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request request

toSession : Model -> Session
toSession model =
    model.session