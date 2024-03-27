module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Events exposing (..)

import Page              exposing (Page)
-- import Page.Register    as Register
import Page.Login       as Login
import Page.CodeProblem as CodeProblem
import Json.Decode as Decode exposing (Value)

-- MODEL

type Model
    = Login Login.Model
    | CodeProblem CodeProblem.Model

init : () -> (Model, Cmd Msg)
-- init _ = let (loginModel, loginCmd) =
--                     CodeProblem.init
--             in
--             (CodeProblem loginModel, Cmd.map GotCodeProblemMsg loginCmd)
init _ = let (loginModel, loginCmd) =
                    Login.init
            in
            (Login loginModel, loginCmd)

-- UPDATE

type Msg 
    = GotLoginMsg Login.Msg
    | GotCodeProblemMsg CodeProblem.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (GotLoginMsg subMsg , Login login) -> Login.update subMsg login |> updateWith Login GotLoginMsg model
        (GotCodeProblemMsg subMsg , CodeProblem cp) -> CodeProblem.update subMsg cp |> updateWith CodeProblem GotCodeProblemMsg model
        ( _, _ ) -> ( model, Cmd.none )

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

-- VIEW

view : Model -> Document Msg
view model =
    let viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login)

        CodeProblem codeProblem ->
            viewPage Page.CodeProblem GotCodeProblemMsg (CodeProblem.view codeProblem)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


-- MAIN

extractBody : Document Msg -> Html Msg
extractBody document =
    div [] document.body

main : Program () Model Msg
main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = \model -> view model |> extractBody
    }