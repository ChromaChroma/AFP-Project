module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

-- import Api exposing (Cred)
-- import Browser.Navigation as Nav
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Encode as Encode 
import Task exposing (Task)

-- MODEL

type alias Model =
  { user: User
  }

type alias User =
  { username : String
  , password : String
  }

init : (Model, Cmd msg)
init = 
    ({ user    =
            { username = ""
            , password = ""
            }
     }, Cmd.none)

-- UPDATE

type Msg
  = SetUsername String
  | SetPassword String
  | SubmitLogin
  | CompletedLogin (Result Http.Error User)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitLogin ->
            let request = submitRequest model.user
            in ( model, request)


        SetUsername username ->
            updateCred (\user -> { user | username = username }) model

        SetPassword password ->
            updateCred (\user -> { user | password = password }) model

        CompletedLogin (Ok user) ->
            ( model
            , Cmd.none
            )

        CompletedLogin (Err _) -> (model, Cmd.none)


updateCred : (User -> User) -> Model -> ( Model, Cmd Msg )
updateCred transform model =
    ( { model | user = transform model.user }, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
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

userDecoder : Decoder User
userDecoder =
  Decode.map2 User
    (Decode.field "username" Decode.string)
    (Decode.field "password" Decode.string)

-- handleResponse : Result Http.Error User -> Msg
-- handleResponse result =
--     case result of
--         Ok user ->
--             -- Handle successful response here, perhaps by sending a success message
--             -- For example:
--             ValidationSuccessful user

--         Err _ ->
--             -- Handle error response here, perhaps by sending an error message
--             -- For example:
--             ValidationFailed "error"

submitRequest : User -> Cmd Msg
submitRequest user =
    Http.post
        { body = Http.jsonBody (userEncoder user)
         , expect = Http.expectJson CompletedLogin userDecoder
        , url = "http://local-host:8000/validate"
        }

-- sendRequest : User -> Cmd Msg
-- sendRequest data =
--     postRequest data
--         |> Task.attempt CompletedLogin

-- postRequest : User -> Cmd Msg
-- postRequest user =
--     let
--         url = "https://example.com/api/endpoint"
--         body = userEncoder user
--         requestConfig =
--             { expect = Http.expectJson CompletedLogin
--             , body = Http.jsonBody body
--             , url = url
--             }
--     in
--     Http.post requestConfig