module Documents.Homepage exposing (Homepage, decodeHomepage)

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
    , select : Field.Select

    --    , group : CardGroup
    }


type alias CardGroup =
    { title : StructuredText
    , image : ImageViews
    , linkText : StructuredText
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
        |> required "select" Field.select



--    |> required "group" decodeCardGroup


decodeCardGroup : Decoder Group CardGroup
decodeCardGroup =
    decode CardGroup
        |> Group.required "title" structuredText
        |> Group.required "image" image
        |> Group.required "description" structuredText
