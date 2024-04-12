module Page.Login exposing (Model, Msg, init, subscriptions, update, view, toSession)

import Browser                           exposing (Document)
import Html                              exposing (..)
import Html.Attributes                   exposing (..)
import Html.Events                       exposing (..)
import Debug                             exposing (log)
import Task                              exposing (Task)
import Http
import Browser.Navigation as Nav
import Page.CodeProblem   as CodeProblem
import Utils.Route        as Route       exposing (pushUrl)
import Utils.Error        as Error       exposing (errorToStr)
import Utils.Session      as Session     exposing (getNavKey)
import Utils.Types                       exposing (..)
import Utils.Transcoder                  exposing (..)


-- MODEL

{-| Represents an alias for the LoginModel
-}
type alias Model = LoginModel

{-| This function initilize the login page.
-}
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

{-| Represents the different messages that can be called.
-}
type Msg
  = UpdateInput    Field String
  | SubmitLogin
  | CompletedLogin (Result Http.Error Cred)
  | GotSession     Session

{-| This function updates the model state based on the received messages.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitLogin                   ->
            let 
                request = submitRequest model.user
            in 
            ( model
            , request
            )

        UpdateInput Username username ->
            updateCred (\user -> { user | username = username }) model

        UpdateInput Password password ->
            updateCred (\user -> { user | password = password }) model

        CompletedLogin (Ok cred)      ->
            let
                updateModel = { model | session = Authenticated (getNavKey model.session) cred }
                request =  Route.pushUrl CodingProblemRoute (getNavKey updateModel.session)
            in
            ( updateModel
            , Cmd.batch [request, Session.store cred]
            ) 

        CompletedLogin (Err errorMsg) ->
            (model, Cmd.none) 
                |> Debug.log (errorToStr errorMsg)

        GotSession session            ->
            ( { model | session = session }
            , Route.pushUrl CodingProblemRoute (getNavKey model.session)
            )

{-| This function updates the user details of the model state.
-}
updateCred : (User -> User) -> Model -> ( Model, Cmd Msg )
updateCred transform model =
    ( { model | user = transform model.user }
    , Cmd.none
    )


-- SUBSCRIPTIONS

{-| This function listens to on local storage change events,
    to update the session with the current authentication information.
-}
subscriptions : Model -> Sub Msg
subscriptions model = 
    Session.changes GotSession (getNavKey model.session)


-- VIEW

{-| This function defines the view of the login page.
-}
view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div [ class "login-container" ]
            [ h2 [] [ text "Login" ]
            , Html.form [ class "login-form", onSubmit SubmitLogin ]
                [ label [ for "username" ] [ text "Username" ]
                , input [ type_       "text"
                                      , id "username"
                        , name        "username"
                        , placeholder "Enter your username"
                        , value       model.user.username
                        , onInput     (UpdateInput Username) 
                        ] []
                , label [ for "password" ] [ text "Password" ]
                , input [ type_       "password"
                        , id          "password"
                                      , name "password"
                        , placeholder "Enter your password"
                        , value       model.user.password
                        , onInput     (UpdateInput Password) 
                        ] []
                , button [ type_ "submit" ] [ text "Login" ]
                ]
            ]
        }


-- HTTP

{-| This function sends the submitted user login data to the server
-}
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


-- EXPORT

{-| This function returs the session of the model state.
-}
toSession : Model -> Session
toSession model =
    model.session