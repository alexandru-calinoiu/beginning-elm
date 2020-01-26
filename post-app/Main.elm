module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, h3, text)
import Page.EditPost as EditPost
import Page.ListPosts as ListPosts
import Route exposing (Route)
import Url exposing (Url)


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage
        ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Posts ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )

                Route.Post postId ->
                    let
                        ( pageModel, pageCmds ) =
                            EditPost.init postId model.navKey
                    in
                    ( EditPage pageModel, Cmd.map EditPageMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChange
        }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListPosts.Model
    | EditPage EditPost.Model


type Msg
    = ListPageMsg ListPosts.Msg
    | LinkClicked UrlRequest
    | UrlChange Url
    | EditPageMsg EditPost.Msg


view : Model -> Document Msg
view model =
    { title = "Post App"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListPosts.view pageModel
                |> Html.map ListPageMsg

        EditPage pageModel ->
            EditPost.view pageModel
                |> Html.map EditPageMsg


notFoundView : Html Msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatePageModel, updateCmd ) =
                    ListPosts.update subMsg pageModel
            in
            ( { model | page = ListPage updatePageModel }
            , Cmd.map ListPageMsg updateCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChange url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditPageMsg subMsg, EditPage pageModel ) ->
            let
                ( updatePageModel, updatedCmd ) =
                    EditPost.update subMsg pageModel
            in
            ( { model | page = EditPage updatePageModel }, Cmd.map EditPageMsg updatedCmd )

        ( _, _ ) ->
            ( model, Cmd.none )


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch post at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]