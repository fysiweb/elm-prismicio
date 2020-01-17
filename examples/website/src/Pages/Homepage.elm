module Pages.Homepage exposing (view)

import Documents.Homepage exposing (Homepage)
import Documents.Menu exposing (Menu)
import Html exposing (Html)
import Html.Attributes as Html
import Pages.Views exposing (viewBodySlice, viewHeader)
import Prismic.Field as Prismic


view : Prismic.LinkResolver msg -> Menu -> Homepage -> Html msg
view linkResolver menu homepage =
    Html.div [ Html.class "homepage" ]
        [ Html.text "SSSSSSSSSSSS"
        ]
