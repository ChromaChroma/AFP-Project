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
import Session                           exposing (Session,Cred, getNavKey)

-- TODO: fix routing, handled access correctly (Using session) & refreshing of token, show output of code, abstract Http with endpoints, update css styling, register page, choce codeproblem page

-- MODEL

type alias Model =
  { session : Session 
  , user    : User
  }

type alias User =
  { username : String
  , password : String
  }

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

type Field
  = Username
  | Password

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
                updateModel = { model | session = Session.Authenticated (getNavKey model.session) cred }
                request =  Route.pushUrl Route.CodeProblem (getNavKey updateModel.session)
            in
            ( updateModel
            , request
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
    -- Session.changes GotSession (getNavKey model.session)

-- VIEW

-- view : Model -> { title : String, content : Html Msg }
-- view model =
--     { title   = "Login"
--     , content =
--         div [ class "cred-page" ]
--             [ div [ class "container page" ]
--                 [ div [ class "row" ]
--                     [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
--                         [ h1 [ class "text-xs-center" ] [ text "Sign in" ]
--                         , viewUserLogin model.user
--                         ]
--                     ]
--                 ]
--             ]
--     }

-- viewUserLogin : User -> Html Msg
-- viewUserLogin form =
--     Html.form [ onSubmit SubmitLogin ]
--         [ fieldset [ class "form-group" ]
--             [ input
--                 [ class "form-control form-control-lg"
--                 , placeholder "username"
--                 , onInput SetUsername
--                 , value form.username
--                 ]
--                 []
--             ]
--         , fieldset [ class "form-group" ]
--             [ input
--                 [ class "form-control form-control-lg"
--                 , type_ "password"
--                 , placeholder "Password"
--                 , onInput SetPassword
--                 , value form.password
--                 ]
--                 []
--             ]
--         , button [ class "btn btn-lg btn-primary pull-xs-right" ]
--             [ text "Sign in" ]
--         ]

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "login-container" ]
            [ h2 [] [ text "Login" ]
            , Html.form [ class "login-form", onSubmit SubmitLogin ]
                [ label [ for "username" ] [ text "Username" ]
                , input [ type_ "text", id "username", name "username", placeholder "Enter your username", value model.user.username, onInput (UpdateInput Username) ] []
                , label [ for "password" ] [ text "Password" ]
                , input [ type_ "password", id "password", name "password", placeholder "Enter your password", value model.user.password, onInput (UpdateInput Password) ] []
                , button [ type_ "submit" ] [ text "Login" ]
                ]
            ]
        }

-- viewForm : Model -> Html Msg
-- viewForm model =



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
            [ Http.header "Access-Control-Allow-Origin" "*",
              Http.header "Accept" "*/*",
              Http.header "Authorization" "Bearer eyJhbGciOiJIUzUxMiJ9.eyJhdWQiOiJhY2Nlc3MiLCJleHAiOjEuNzExNTYwMjc2Mjk1MjM3NTZlOSwiaWF0IjoxLjcxMTU1OTM3NjI5NTIzNzU2ZTksInN1YiI6IjA4YTJjM2Q5LWI3ZWMtNDhlNS04ZjQwLTNhOTQyYWQwMTEzMCJ9.5ddVeYFeCJ9KaUrpUVc6iqx1r30viqLqqZvGEUCi7GneW0UEO5oYLNF4c-jkRV5u6cM8HyyE0AOuv10fadrZWA"
            ]

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