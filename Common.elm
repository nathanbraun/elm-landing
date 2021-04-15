module Common exposing
    ( gray
    , noPadding
    , noto
    , source
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
import Widget
import Widget.Customize as Customize
import Widget.Material as Material


gray : Color
gray =
    Element.rgb255 149 165 166


noto : Element.Attribute msg
noto =
    Font.family [ Font.typeface "Noto Sans JP" ]


viewHeader : Int -> Device -> List String -> Maybe (List String) -> Element msg
viewHeader h device desktopText mobileText =
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
        [ Font.size fontSize
        , Font.semiBold
        , centerX
        , Font.center
        , spacing 10
        , Font.family [ Font.typeface "Noto Sans JP" ]
        ]
        (List.map (\x -> paragraph [] [ text x ]) textList)


viewText : Device -> Int -> String -> Element msg
viewText device max text_ =
    column [ centerX, width (fill |> maximum max) ]
        [ paragraph
            [ source
            , Font.center
            , Font.color (Element.rgb255 149 165 166)
            ]
            [ text text_ ]
        ]


source : Element.Attribute msg
source =
    Font.family [ Font.typeface "Source Serif Pro" ]


viewEmail : Device -> { model | email : String } -> (String -> msg) -> Element msg
viewEmail device model msg =
    let
        desktopView =
            column [ width fill, paddingXY 0 50, spacing 10 ]
                [ row [ centerX, spacing 20, width (fill |> maximum 450) ]
                    [ el [ width fill ] (viewLoc model msg)
                    , el [ centerX ] (viewButton "Try It!")
                    ]
                ]

        mobileView =
            column [ centerX, spacing 10, width (fill |> maximum 500), paddingXY 0 20 ]
                [ el [ width fill, paddingXY 10 0 ] (viewLoc model msg)
                , el [ centerX, paddingXY 0 0 ] (viewButton "Try It!")
                ]
    in
    case ( device.class, device.orientation ) of
        ( Desktop, _ ) ->
            desktopView

        ( _, Landscape ) ->
            desktopView

        _ ->
            mobileView


viewLoc : { model | email : String } -> (String -> msg) -> Element msg
viewLoc model msg =
    Widget.textInput (Material.textInput Material.defaultPalette |> Customize.elementRow [ width fill ])
        { chips = []
        , text = model.email
        , placeholder =
            Just <|
                Input.placeholder [] <|
                    el [ source ] (Element.text "Email")
        , onChange = msg
        , label = "Location"
        }


viewButton : String -> Element msg
viewButton label =
    Input.button
        [ centerX ]
        { onPress = Nothing
        , label =
            el
                [ Border.width 1
                , Border.rounded 5
                , paddingXY 30 15
                , Background.color (rgb255 52 152 219)
                , Font.color (rgb255 255 255 255)
                , Font.family
                    [ Font.typeface "Noto Sans JP"
                    ]
                ]
                (text label)
        }


viewMaxImage : Int -> String -> String -> Element ms
viewMaxImage max src desc =
    image
        [ centerX
        , Element.alignTop
        , width (fillPortion 1 |> maximum max)
        ]
        { src = src
        , description = desc
        }


viewImageText : String -> String -> String -> Element msg
viewImageText caption src desc =
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
            viewMaxImage 250 src desc
        ]


viewLogos : Element msg
viewLogos =
    column
        [ width fill
        , paddingXY 0 40
        , Background.color (Element.rgb255 236 240 241)
        , spacing 20
        , Font.color gray
        ]
        [ el [ centerX, noto ] (text "MADE WITH:")
        , row
            [ spacing 20
            , centerX
            ]
            [ viewMaxImage 40 "images/elm.png" "Elm Logo"
            , viewMaxImage 40 "images/graphql.png" "GraphQL Logo"
            , viewMaxImage 40 "images/google.png" "Google Logo"
            ]
        ]


noPadding =
    { top = 0, bottom = 0, right = 0, left = 0 }


viewGrid : Device -> Element msg
viewGrid device =
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
    column [ centerX, source, paddingXY 5 0 ]
        [ wrappedRow []
            [ el style (viewImageText "1. Read your book, either on your Kindle or in print." "/images/f3wt1_58.jpg" "1")
            , el style (viewImageText "2. When you spot something interesting, type the page number and record a quick audio note." "/images/record_screenshot.png" "2")
            ]
        , wrappedRow []
            [ el style (viewImageText "3. Need to follow up? Record as many subnotes as you want." "/images/sub_screenshot.png" "2")
            , el style (viewImageText "4. Export your notes to plain text for editing when you're done." "/images/final_58.jpg" "2")
            ]
        ]
