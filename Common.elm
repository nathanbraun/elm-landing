module Common exposing
    ( Fonts
    , Image
    , viewEmail
    , viewGrid
    , viewHeader
    , viewImageText
    , viewLogos
    , viewMaxImage
    , viewText
    )

import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Loading exposing (LoaderType(..), defaultConfig, render)
import MailerLite exposing (MailerLiteResponse)
import RemoteData exposing (RemoteData(..))
import Widget
import Widget.Customize as Customize
import Widget.Material as Material


type alias Image =
    { src : String
    , desc : String
    }


type alias Fonts msg =
    { main : Element.Attribute msg
    , secondary : Element.Attribute msg
    }


viewHeader :
    Int
    -> List (Element.Attribute msg)
    -> Device
    -> List String
    -> Maybe (List String)
    -> Element msg
viewHeader h style device desktopText mobileText =
    let
        textList =
            case ( device.class, device.orientation ) of
                ( Desktop, _ ) ->
                    desktopText

                ( _, Landscape ) ->
                    desktopText

                _ ->
                    mobileText
                        |> Maybe.withDefault
                            [ String.join " "
                                desktopText
                            ]

        fontSize =
            case h of
                1 ->
                    56

                2 ->
                    40

                _ ->
                    20
    in
    column
        ([ Font.size fontSize
         , centerX
         , Font.center
         , spacing 10
         ]
            ++ style
        )
        (List.map (\x -> paragraph [] [ text x ]) textList)


viewText : List (Element.Attribute msg) -> Device -> Int -> String -> Element msg
viewText style device max text_ =
    column [ centerX, width (fill |> maximum max) ]
        [ paragraph
            ([ Font.center ] ++ style)
            [ text text_ ]
        ]


viewEmail :
    Fonts msg
    -> String
    -> Device
    -> { model | email : String, status : RemoteData Http.Error (Result Http.Error MailerLiteResponse) }
    -> (String -> msg)
    -> msg
    -> Element msg
viewEmail fonts buttonText device model updateMsg submitMsg =
    let
        emailInput_ =
            viewLoc fonts.secondary model updateMsg

        ( button, emailInput ) =
            case model.status of
                NotAsked ->
                    ( viewButton submitMsg
                        fonts.main
                        (text buttonText)
                    , emailInput_
                    )

                Loading ->
                    ( viewButton submitMsg
                        fonts.main
                        (Loading.render
                            Circle
                            -- LoaderType
                            { defaultConfig | size = 20 }
                            -- Config
                            Loading.On
                            |> Element.html
                        )
                    , emailInput_
                    )

                Success response ->
                    ( none
                    , paragraph [ fonts.secondary, Font.center, centerX, width fill ]
                        [ text "You've successfully signed up with ", text model.email, text "." ]
                    )

                _ ->
                    ( viewButton submitMsg
                        fonts.main
                        (text buttonText)
                    , emailInput_
                    )

        desktopView =
            column [ width fill, paddingXY 0 50, spacing 10 ]
                [ row [ centerX, spacing 20, width (fill |> maximum 450) ]
                    [ el [ width fill ]
                        emailInput
                    , el [ centerX ]
                        button
                    ]
                ]

        mobileView =
            column
                [ centerX
                , spacing 10
                , width (fill |> maximum 500)
                , paddingXY 0 20
                ]
                [ el [ width fill, paddingXY 10 0 ]
                    emailInput
                , el [ centerX, paddingXY 0 0 ]
                    button
                ]
    in
    case ( device.class, device.orientation ) of
        ( Desktop, _ ) ->
            desktopView

        ( _, Landscape ) ->
            desktopView

        _ ->
            mobileView


viewLoc :
    Element.Attribute msg
    -> { model | email : String }
    ->
        (String
         -> msg
        )
    -> Element msg
viewLoc font model msg =
    Widget.textInput (Material.textInput Material.defaultPalette |> Customize.elementRow [ width fill ])
        { chips = []
        , text = model.email
        , placeholder =
            Just <|
                Input.placeholder [] <|
                    el [ font ] (Element.text "Email")
        , onChange = msg
        , label = "Location"
        }


viewButton : msg -> Element.Attribute msg -> Element msg -> Element msg
viewButton msg font label =
    Input.button
        [ centerX ]
        { onPress = Just msg
        , label =
            el
                [ Border.width 1
                , Border.rounded 5
                , paddingXY 30 15
                , Background.color (rgb255 52 152 219)
                , Font.color (rgb255 255 255 255)
                , font
                , Font.center
                , width (px 175)
                ]
                label
        }


viewMaxImage : Int -> Image -> Element ms
viewMaxImage max img =
    image
        [ centerX
        , Element.alignTop
        , width (fillPortion 1 |> maximum max)
        ]
        { src = img.src
        , description = img.desc
        }


viewImageText : String -> Image -> Element msg
viewImageText caption img =
    Widget.itemList
        (Material.cardColumn Material.defaultPalette)
        [ Widget.asItem <|
            el
                [ width fill
                , height (fill |> minimum 150)
                ]
                (paragraph
                    [ width (fill |> maximum 250)
                    , centerX
                    , Font.center
                    , centerY
                    ]
                    [ text caption
                    ]
                )
        , Widget.divider (Material.middleDivider Material.defaultPalette)
        , Widget.asItem <|
            viewMaxImage 250 (Image img.src img.desc)
        ]


viewLogos : List (Element.Attribute msg) -> List Image -> Element msg
viewLogos textStyle logos =
    column
        [ width fill
        , paddingXY 0 40
        , Background.color (Element.rgb255 236 240 241)
        , spacing 20
        ]
        [ el ([ centerX ] ++ textStyle) (text "MADE WITH:")
        , row
            [ spacing 20
            , centerX
            ]
            (List.map (\{ src, desc } -> viewMaxImage 40 (Image src desc)) logos)
        ]


viewGrid : Element.Attribute msg -> Device -> Element msg
viewGrid font device =
    let
        style =
            case ( device.class, device.orientation ) of
                ( Desktop, _ ) ->
                    [ alignTop, padding 5 ]

                ( _, Landscape ) ->
                    [ alignTop, padding 5 ]

                _ ->
                    [ alignTop ]
    in
    column [ centerX, font, paddingXY 5 0 ]
        [ wrappedRow []
            [ el style (viewImageText "1. Read your book, either on your Kindle or in print." (Image "/images/f3wt1_58.jpg" "1"))
            , el style (viewImageText "2. When you spot something interesting, type the page number and record a quick audio note." (Image "/images/record_screenshot.png" "2"))
            ]
        , wrappedRow []
            [ el style (viewImageText "3. Need to follow up? Record as many subnotes as you want." (Image "/images/sub_screenshot.png" "2"))
            , el style (viewImageText "4. Export your notes to plain text for editing when you're done." (Image "/images/final_58.jpg" "2"))
            ]
        ]
