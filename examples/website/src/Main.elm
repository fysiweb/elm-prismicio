module Main exposing (main)

import Browser
import Documents.Homepage
import Documents.Menu
import Documents.Page
import Html exposing (Html)
import Html.Attributes exposing (class, href, src, target)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Pages.Homepage
import Pages.Page
import Pages.Views
import Prismic
import Prismic.Field as Prismic exposing (defaultLinkResolver)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = \model -> { title = "example", body = [ view model ] }
        , subscriptions = always Sub.none
        }


type alias Model =
    { prismic : Prismic.Model
    , currentPage : Page
    , menu : Maybe Documents.Menu.Menu
    , homepage : Maybe Documents.Homepage.Homepage
    , page : Maybe Documents.Page.Page
    }


type Page
    = Homepage
    | Page


init : ( Model, Cmd Msg )
init =
    let
        model =
            { prismic =
                Prismic.init "https://liascript.prismic.io/api"
            , currentPage = Homepage
            , menu = Nothing
            , homepage = Nothing
            , page = Nothing
            }
    in
    ( model, fetchHomePage model.prismic )


type alias PrismicResult =
    Result Prismic.PrismicError ( Prismic.Model, Prismic.Response )


type Msg
    = HomepageResponse PrismicResult
    | NavigateTo Prismic.DocumentReference


linkResolver : Prismic.LinkResolver Msg
linkResolver =
    { defaultLinkResolver
        | resolveDocumentReference =
            \ref ->
                [ onClick (NavigateTo ref), href "#" ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomepageResponse (Ok ( prismic, result )) ->
            let
                newPrismic =
                    Prismic.cache model.prismic prismic
            in
            ( { model
                | prismic =
                    newPrismic
                , homepage =
                    result.results
                        |> List.head
                        |> Maybe.andThen (Prismic.decodeResult Documents.Homepage.decodeHomepage >> Result.toMaybe)
              }
            , Cmd.none
              --fetchHomePage newPrismic
            )

        HomepageResponse (Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            ( model, Cmd.none )

        NavigateTo ref ->
            case ( ref.linkedDocumentType, ref.uid ) of
                ( "homepage", Just "homepage" ) ->
                    ( { model | currentPage = Homepage }, Cmd.none )

                ( "page", Just uid ) ->
                    ( { model | currentPage = Page, page = Nothing }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


fetchHomePage : Prismic.Model -> Cmd Msg
fetchHomePage prismic =
    Prismic.api prismic
        |> Prismic.form "everything"
        |> Prismic.query [ Prismic.at "document.type" "title" ]
        |> Prismic.submit
        |> Task.attempt HomepageResponse


view : Model -> Html Msg
view model =
    Html.div []
        [ model.homepage
            |> Maybe.map (.title >> Pages.Views.asHtml >> Html.div [])
            |> Maybe.withDefault (Html.text "dddd")
        , model.homepage
            |> Maybe.map (.richtext >> Pages.Views.asHtml >> Html.div [])
            |> Maybe.withDefault (Html.text "dddd")
        ]


loading : Html msg
loading =
    Html.text "..."


viewFooter : Html msg
viewFooter =
    Html.footer []
        [ Html.p []
            [ Html.text "Proudly published with "
            , Html.a [ href "https://prismic.io", target "_blank" ]
                [ Html.text "prismic.io"
                ]
            , Html.br [] []
            , Html.a [ href "https://prismic.io", target "_blank" ]
                [ Html.img
                    [ class "footer-logo"
                    , src "https://website-sample.herokuapp.com/images/logo-prismic.svg"
                    ]
                    []
                ]
            ]
        ]
