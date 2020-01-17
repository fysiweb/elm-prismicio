module Documents.Homepage exposing (BodySlice(..), GalleryGroup, GalleryWithTitle, HighlightGroup, Homepage, bodySliceZone, decodeGalleryGroup, decodeHighlightGroup, decodeHomepage)

import Prismic
    exposing
        ( Decoder
        , Document
        , custom
        , decode
        , group
        , map
        , optional
        , required
        , sliceZone
        )
import Prismic.Field as Field
    exposing
        ( Color
        , Date
        , GeoPoint
        , ImageViews
        , Link
        , StructuredText
        , Timestamp
        , color
        , date
        , geoPoint
        , image
        , link
        , structuredText
        , text
        , timestamp
        )
import Prismic.Group as Group exposing (Group)
import Prismic.Slice as Slice
    exposing
        ( Slice
        , labelledV1Slice
        , slice
        , v1Slice
        )


type alias Homepage =
    { title : StructuredText
    , richtext : StructuredText
    , image : ImageViews
    , date : Date
    , time : Timestamp
    , color : Color
    , geo : GeoPoint
    }


type BodySlice
    = Heading StructuredText
    | TextSection (Maybe String) StructuredText
    | Highlight (List HighlightGroup)
    | FullWidthImage ImageViews
    | Gallery (List GalleryGroup)
    | GalleryV2 GalleryWithTitle
    | SingleRepeat (List StructuredText)


type alias HighlightGroup =
    { title : StructuredText
    , headline : StructuredText
    , image : ImageViews
    , link : Maybe Link
    , linkText : Maybe String
    }


type alias GalleryGroup =
    { description : StructuredText
    , image : ImageViews
    }


type alias GalleryWithTitle =
    { title : StructuredText
    , groups : List GalleryGroup
    }


decodeHomepage : Decoder Document Homepage
decodeHomepage =
    decode Homepage
        |> required "title" structuredText
        |> required "richtext" structuredText
        |> required "image" image
        |> required "date" date
        |> required "time" timestamp
        |> required "color" color
        |> required "gps" geoPoint


bodySliceZone : Decoder Slice BodySlice
bodySliceZone =
    Slice.oneOf
        [ v1Slice "heading" Heading (Slice.field structuredText)
        , labelledV1Slice "textSection" TextSection (Slice.field structuredText)
        , v1Slice "highlight" Highlight (Slice.group decodeHighlightGroup)
        , v1Slice "fullWidthImage" FullWidthImage (Slice.field image)
        , v1Slice "gallery" Gallery (Slice.group decodeGalleryGroup)
        , slice "new_image_gallery"
            (Group.field "title" structuredText)
            decodeGalleryGroup
            |> Prismic.map
                (\( title, groups ) -> GalleryV2 (GalleryWithTitle title groups))
        , slice "single_repeat"
            (decode ())
            (Group.field "title" structuredText)
            |> Prismic.map (\( _, texts ) -> SingleRepeat texts)
        ]


decodeHighlightGroup : Decoder Group HighlightGroup
decodeHighlightGroup =
    decode HighlightGroup
        |> Group.required "title" structuredText
        |> Group.required "headline" structuredText
        |> Group.required "image" image
        |> Group.optional "link" (map Just link) Nothing
        |> Group.optional "linkText" (map Just text) Nothing


decodeGalleryGroup : Decoder Group GalleryGroup
decodeGalleryGroup =
    decode GalleryGroup
        |> Group.required "description" structuredText
        |> Group.required "image" image
