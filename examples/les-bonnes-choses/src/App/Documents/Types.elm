module App.Documents.Types exposing (..)

import Prismic.Types exposing (PrismicError, Response, Api, StructuredText, Link, DefaultDocType, ImageField)


type alias Article =
    { content : StructuredText
    , image : ImageField
    , shortLede : StructuredText
    , title : StructuredText
    }


type alias BlogPost =
    { id : String
    , slugs : List String
    , body : StructuredText
    , author : String
    , category : String
    , date : String
    , shortLede : StructuredText
    , relatedPosts : List Link
    , relatedProducts : List Link
    , allowComments : Bool
    }


type alias JobOffer =
    { name : StructuredText
    , contractType : Maybe String
    , service : Maybe String
    , jobDescription : StructuredText
    , profile : StructuredText
    , locations : List Link
    }


type Category
    = Macaron
    | Cupcake
    | Pie


type alias Product =
    { id : String
    , slugs : List String
    , allergens : Maybe String
    , color : String
    , description : StructuredText
    , flavours : List String
    , gallery : List ImageField
    , image : ImageField
    , name : StructuredText
    , price : Float
    , related : List Link
    , shortLede : StructuredText
    , testimonialAuthor : Maybe StructuredText
    , testimonialQuote : Maybe StructuredText
    , tags : List String
    , categories : List Category
    }


type alias Selection =
    { id : String
    , slugs : List String
    , tags : List String
    , name : StructuredText
    , catcherImage : ImageField
    , description : StructuredText
    , image : ImageField
    , price : Float
    , products : List Link
    , shortLede : StructuredText
    }


type alias Store =
    { id : String
    , slugs : List String
    , tags : List String
    , address : String
    , city : String
    , zipcode : String
    , country : String
    , description : StructuredText
    , name : StructuredText
    , image : ImageField
    , monday : List String
    , tuesday : List String
    , wednesday : List String
    , thursday : List String
    , friday : List String
    , saturday : List String
    , sunday : List String
    }