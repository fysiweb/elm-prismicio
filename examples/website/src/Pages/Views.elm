module Pages.Views exposing (asHtml, viewBodySlice, viewHeader)

import Documents.Homepage exposing (BodySlice(..), GalleryGroup, HighlightGroup, Homepage)
import Documents.Menu exposing (Menu)
import Html exposing (Html)
import Html.Attributes as Html
import Prismic.Field as Prismic


asHtml : Prismic.StructuredText -> List (Html msg)
asHtml =
    Prismic.structuredTextAsHtml Prismic.defaultLinkResolver


viewHeader : Prismic.LinkResolver msg -> Menu -> Html msg
viewHeader linkResolver menu =
    let
        viewLink link =
            Html.li []
                [ Html.a
                    (Prismic.resolveLink linkResolver link.link)
                    [ Html.text link.label ]
                ]
    in
    Html.header [ Html.class "site-header" ]
        [ Html.a [ Html.href "./" ]
            [ Html.div
                [ Html.class "logo" ]
                [ Html.text
                    (Prismic.getTexts menu.title)
                ]
            ]
        , Html.nav
            []
            [ Html.ul []
                (List.map viewLink menu.links)
            ]
        ]


viewBodySlice : BodySlice -> Html msg
viewBodySlice bodySlice =
    case bodySlice of
        Heading text ->
            viewHeading text

        TextSection label text ->
            viewTextSection label text

        Highlight groups ->
            viewHighlights groups

        FullWidthImage image ->
            viewFullWidthImage image

        Gallery groups ->
            viewGallery groups

        GalleryV2 gallery ->
            Html.div []
                [ viewHeading gallery.title
                , viewGallery gallery.groups
                ]

        SingleRepeat texts ->
            Html.div [] (List.concatMap asHtml texts)


viewHeading : Prismic.StructuredText -> Html msg
viewHeading text =
    Html.div [] (asHtml text)


viewTextSection : Maybe String -> Prismic.StructuredText -> Html msg
viewTextSection label text =
    let
        sectionClass =
            "text-section-"
                ++ Maybe.withDefault "1col" label
    in
    Html.section
        [ Html.class "content-section"
        , Html.class sectionClass
        ]
        (asHtml text)


viewHighlights : List HighlightGroup -> Html msg
viewHighlights groups =
    let
        viewGroup : HighlightGroup -> Html msg
        viewGroup group =
            Html.section
                [ Html.class "highlight"
                , Html.class "content-section"
                ]
                [ Html.div [ Html.class "highlight-left" ]
                    (List.concat
                        [ asHtml group.title
                        , asHtml group.headline
                        , Maybe.map2
                            (\link linkText ->
                                [ Html.p []
                                    [ Html.a
                                        [ Html.href "todo" ]
                                        --todo: link ]
                                        [ Html.text linkText ]
                                    ]
                                ]
                            )
                            group.link
                            group.linkText
                            |> Maybe.withDefault []
                        ]
                    )
                , Html.div [ Html.class "highlight-right" ]
                    [ Html.img [ Html.src group.image.main.url ] [] ]
                ]
    in
    Html.div []
        (List.map viewGroup groups)


viewFullWidthImage : Prismic.ImageViews -> Html msg
viewFullWidthImage image =
    Html.section [ Html.class "full-width-image", Html.class "content-section" ]
        [ Html.img [ Html.src image.main.url ] [] ]


viewGallery : List GalleryGroup -> Html msg
viewGallery groups =
    let
        viewItem : GalleryGroup -> Html msg
        viewItem item =
            Html.div [ Html.class "gallery-item" ]
                (Html.img [ Html.src item.image.main.url ] []
                    :: asHtml item.description
                )
    in
    Html.section [ Html.class "gallery", Html.class "content-section" ]
        (List.map viewItem groups)
