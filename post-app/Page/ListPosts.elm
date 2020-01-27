module Page.ListPosts exposing (Model, Msg, init, update, view)

import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (href, type_)
import Html.Events exposing (onClick)
import Http
import Post exposing (Post, PostId, idToString, postsDecoder)
import RemoteData exposing (WebData)


type alias Model =
    { posts : WebData (List Post)
    , deleteError : Maybe String
    }


type Msg
    = SendHttpRequest
    | DataReceived (WebData (List Post))
    | DeletePost Post.PostId
    | PostDeleted (Result Http.Error String)


initialModel : Model
initialModel =
    { posts = RemoteData.Loading
    , deleteError = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchPosts )


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:5019/posts"
        , expect =
            postsDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        }


deletePost : PostId -> Cmd Msg
deletePost postId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:5019/posts/" ++ idToString postId
        , body = Http.emptyBody
        , expect = Http.expectString PostDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

        DataReceived response ->
            ( { model | posts = response }, Cmd.none )

        DeletePost postId ->
            ( model, deletePost postId )

        PostDeleted (Ok _) ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

        PostDeleted (Err error) ->
            ( { model | deleteError = Just (buildErrorMessage error) }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Refresh posts" ]
        , br [] []
        , br [] []
        , a [ href "posts/new" ] [ text "Create new post" ]
        , viewPostsOrError model
        , viewDeleteError model.deleteError
        ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading ..." ]

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)

        RemoteData.Success posts ->
            viewPosts posts


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Coun't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table []
            (viewTableHeader :: List.map viewPost posts)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th [] [ text "ID" ]
        , th [] [ text "Title" ]
        , th [] [ text "Author" ]
        , th [] [ text "Actions" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    let
        postPath =
            "/posts/" ++ Post.idToString post.id
    in
    tr []
        [ td [] [ text (idToString post.id) ]
        , td [] [ text post.title ]
        , td [] [ a [ href post.authorUrl ] [ text post.authorName ] ]
        , td [] [ a [ href postPath ] [ text "Edit" ] ]
        , td []
            [ button [ type_ "button", onClick (DeletePost post.id) ]
                [ text "Delete" ]
            ]
        ]


viewDeleteError : Maybe String -> Html Msg
viewDeleteError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't delete post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
