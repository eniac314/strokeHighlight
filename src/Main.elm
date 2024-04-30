module Main exposing (..)

import Base64
import Browser
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html
import Html.Attributes as HtmlAttr
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import List.Extra
import Parser exposing (..)
import String.Extra
import Svg.String as Svg exposing (Svg, node, svg, toHtml)
import SvgParser
import SvgParserVendored exposing (..)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { kanji : Maybe Char
    , kanjiInputBuffer : Maybe String
    , strokeInputBuffer : Maybe String
    , highlightedStrokes : List Int
    , animate : Bool
    , parsedSvg :
        { svg : List SvgNode
        , styleBlock : String
        }
    }


type Msg
    = StrokeInput String
    | KanjiInput String
    | GotSvg (Result Http.Error String)
    | NoOp


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { kanji = Nothing
      , kanjiInputBuffer = Nothing
      , strokeInputBuffer = Nothing
      , highlightedStrokes = []
      , animate = True
      , parsedSvg =
            { svg = []
            , styleBlock = ""
            }
      }
      --|> toggleAnimation
    , Cmd.none
    )



-------------------------------------------------------------------------------


view model =
    Element.layout [] <|
        column
            [ spacing 20
            , padding 15
            , Font.size 16
            , width fill
            ]
            [ column [ width fill, spacing 15, Font.size 16 ]
                [ Input.text [ width (px 30), paddingXY 7 5 ]
                    { onChange = KanjiInput
                    , text = model.kanjiInputBuffer |> Maybe.withDefault ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Kanji input")
                    }
                , Input.text [ width (px 200), paddingXY 7 5 ]
                    { onChange = StrokeInput
                    , text = model.strokeInputBuffer |> Maybe.withDefault ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Index des traits à colorier")
                    }
                ]
            , render (colorize model)
            ]


render : Model -> Element.Element msg
render model =
    case ( model.parsedSvg.svg, model.kanji ) of
        ( comment :: [ SvgElement { name, attributes, children } ], Just kanji ) ->
            let
                svg =
                    Svg.svg (List.map toAttribute attributes)
                        (List.map nodeToSvg
                            (comment
                                :: SvgElement
                                    { name = "style"
                                    , attributes = []
                                    , children = [ SvgText model.parsedSvg.styleBlock ]
                                    }
                                :: children
                            )
                        )

                reparse =
                    Svg.toString 0 svg
                        |> extractStyleBlock
                        --|> Debug.log ""
                        |> (\( svg_, sb ) ->
                                { svg = SvgParser.parseToNodes svg_ |> Result.withDefault []
                                , styleBlock = sb
                                }
                           )
                        |> (\parsed ->
                                case parsed.svg of
                                    [ SvgParser.SvgElement svgEl ] ->
                                        SvgParser.SvgElement
                                            { name = "svg"
                                            , attributes = ( "viewBox", "0 0 1024 1024" ) :: List.filter (\( a, v ) -> a /= "view-box") svgEl.attributes
                                            , children =
                                                SvgParser.SvgElement
                                                    { name = "style"
                                                    , attributes = []
                                                    , children = [ SvgParser.SvgText parsed.styleBlock ]
                                                    }
                                                    :: svgEl.children
                                            }
                                            |> SvgParser.nodeToSvg

                                    _ ->
                                        --let
                                        --    d =
                                        --        Debug.log "" parsed
                                        --in
                                        Html.text "erreur"
                           )
            in
            column []
                [ el [ width (px 400), height (px 400) ] (html <| reparse)
                , el
                    [ alignRight
                    , Font.color <| rgb255 0 0 200
                    , Font.underline
                    ]
                    (html <|
                        Html.a
                            [ HtmlAttr.href <|
                                "data:application/octet-stream;charset=utf-16le;base64,"
                                    ++ Base64.encode (Svg.toString 0 svg)
                            , HtmlAttr.download ("results-" ++ String.fromChar kanji ++ ".svg")
                            , HtmlAttr.style "text-decoration" "inherit"
                            , HtmlAttr.style "font-family" "monospace"
                            ]
                            [ Html.text ("results-" ++ String.fromChar kanji ++ ".svg") ]
                    )
                ]

        _ ->
            Element.none



-------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StrokeInput s ->
            ( { model
                | strokeInputBuffer = mbStr s
                , highlightedStrokes =
                    case Parser.run strokeRange s of
                        Ok xs ->
                            xs

                        _ ->
                            String.split "," s
                                |> List.filterMap String.toInt
              }
            , Cmd.none
            )

        KanjiInput s ->
            case String.toList s |> List.head of
                Just kc ->
                    ( { model
                        | kanjiInputBuffer = mbStr s
                        , kanji = Just kc
                      }
                    , getSvg kc
                    )

                Nothing ->
                    ( { model
                        | kanjiInputBuffer = mbStr s
                      }
                    , Cmd.none
                    )

        GotSvg res ->
            case res of
                Ok svg ->
                    ( { model
                        | parsedSvg =
                            extractStyleBlock svg
                                |> (\( svg_, sb ) ->
                                        { svg = parseToNodes svg_ |> Result.withDefault []
                                        , styleBlock = sb
                                        }
                                   )
                      }
                        |> toggleAnimation
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


mbStr s =
    if s == "" then
        Nothing

    else
        Just s


strokeRange =
    Parser.succeed List.range
        |. spaces
        |= int
        |. spaces
        |. token "-"
        |. spaces
        |= int



-------------------------------------------------------------------------------


getSvg k =
    let
        filemame =
            Char.toCode k |> String.fromInt |> (\n -> n ++ ".svg")
    in
    Http.get
        { url = "/svgsJa/" ++ filemame
        , expect = Http.expectString GotSvg
        }



-------------------------------------------------------------------------------


extractStyleBlock s =
    let
        flat =
            String.replace "\n" "" s
                |> String.replace "'" "\""

        prefix =
            String.Extra.leftOf "<style>" flat

        --|> Debug.log "prefix"
        suffix =
            String.Extra.rightOfBack "</style>" flat

        styleBlock =
            if String.contains "CDATA" flat then
                String.Extra.rightOf "<style>" flat
                    |> String.Extra.leftOf "</style>"
                    |> String.dropLeft (String.length "<![CDATA[")
                    |> String.dropRight (String.length "]]>")

            else
                String.Extra.rightOf "<style>" flat
                    |> String.Extra.leftOf "</style>"
    in
    ( prefix ++ suffix, styleBlock )


toggleAnimation model =
    let
        newStyleBlock =
            if String.contains "--t:0.8s;" model.parsedSvg.styleBlock then
                String.replace "--t:0.8s;" "--t:0s;" model.parsedSvg.styleBlock

            else
                String.replace "--t:0s;" "--t:0.8s;" model.parsedSvg.styleBlock

        newSvg =
            List.map
                (\node ->
                    case node of
                        SvgElement el ->
                            SvgElement <|
                                { el
                                    | children =
                                        List.map
                                            (\c ->
                                                case c of
                                                    SvgElement el_ ->
                                                        SvgElement
                                                            { el_
                                                                | attributes =
                                                                    List.map
                                                                        newNodeStyle
                                                                        el_.attributes
                                                            }

                                                    _ ->
                                                        c
                                            )
                                            el.children
                                }

                        other ->
                            other
                )
                model.parsedSvg.svg

        newNodeStyle attr =
            case attr of
                ( "style", v ) ->
                    ( "style"
                    , Parser.run styles v
                        |> Result.withDefault []
                        |> List.map
                            (\s ->
                                case s of
                                    Anim d ->
                                        Anim 0

                                    _ ->
                                        s
                            )
                        |> stylesToStr
                    )

                _ ->
                    attr
    in
    { model
        | parsedSvg =
            { styleBlock = newStyleBlock
            , svg = newSvg
            }
    }


colorize model =
    case model.parsedSvg.svg of
        (SvgComment comment) :: (SvgElement el) :: [] ->
            let
                nbrStrokes =
                    List.Extra.takeWhile
                        (\c ->
                            case c of
                                SvgElement { name } ->
                                    name == "path"

                                _ ->
                                    False
                        )
                        el.children
                        |> List.length

                strokeElems =
                    List.reverse el.children
                        |> List.take nbrStrokes
                        |> List.reverse

                newStrokeElems =
                    List.indexedMap
                        (\index node ->
                            case node of
                                SvgElement el_ ->
                                    SvgElement <|
                                        { el_
                                            | attributes =
                                                List.map
                                                    (newStrokeStyle
                                                        (List.member (index + 1) strokes)
                                                    )
                                                    el_.attributes
                                        }

                                other ->
                                    other
                        )
                        strokeElems

                newStrokeStyle highLighted attr =
                    case attr of
                        ( "style", v ) ->
                            ( "style"
                            , Parser.run styles v
                                |> Result.withDefault []
                                |> List.filter
                                    (\s ->
                                        case s of
                                            Anim _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                |> (\ss ->
                                        if highLighted then
                                            ss ++ [ Stroke 255 0 0 ]

                                        else
                                            ss
                                   )
                                |> stylesToStr
                            )

                        _ ->
                            attr

                strokes =
                    model.highlightedStrokes

                --List.filter (\sn -> not (sn > nbrStrokes) model.highlightedStrokes)
            in
            { model
                | parsedSvg =
                    { styleBlock = model.parsedSvg.styleBlock
                    , svg =
                        SvgComment comment
                            :: [ SvgElement
                                    { el
                                        | children =
                                            (List.reverse el.children
                                                |> List.drop nbrStrokes
                                                |> List.reverse
                                            )
                                                ++ newStrokeElems
                                    }
                               ]
                    }
            }

        _ ->
            model


type Style
    = Anim Float
    | Stroke Int Int Int


stylesToStr ss =
    List.map styleToStr ss
        |> String.join " "


styleToStr s =
    case s of
        Anim f ->
            "--d:" ++ String.fromFloat f ++ "s;"

        Stroke r g b ->
            "stroke: rgb(" ++ String.fromInt r ++ ", " ++ String.fromInt g ++ ", " ++ String.fromInt g ++ ");"


styles =
    many style


style =
    oneOf
        [ anim |> backtrackable
        , stroke
        ]


anim : Parser Style
anim =
    Parser.succeed Anim
        |. token "--d:"
        |= float
        |. token "s;"


stroke : Parser Style
stroke =
    Parser.succeed Stroke
        |. spaces
        |. token "stroke: rgb("
        |= int
        |. token ", "
        |= int
        |. token ", "
        |= int
        |. token ");"



-------------------------------------------------------------------------------


many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p

        --|. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse vs))
        ]



-------------------------------------------------------------------------------


test =
    String.Extra.leftOf "Derived" (String.replace "\n" "" svgSample)


svgSample =
    """<!--
AnimCJK 2016-2022 Copyright FM&SH - https://github.com/parsimonhi/animCJK
Derived from:
    MakeMeAHanzi project - https://github.com/skishore/makemeahanzi
    Arphic PL KaitiM GB font
    Arphic PL UKai font
You can redistribute and/or modify this file under the terms of the Arphic Public License
as published by Arphic Technology Co., Ltd.
You should have received a copy of this license along with this file.
If not, see http://ftp.gnu.org/non-gnu/chinese-fonts-truetype/LICENSE.
-->
<svg id="z20491" class="acjk" version="1.1" viewBox="0 0 1024 1024" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<style>
<![CDATA[
@keyframes zk {
    to {
        stroke-dashoffset:0;
    }
}
svg.acjk path[clip-path] {
    --t:0.8s;
    animation:zk var(--t) linear forwards var(--d);
    stroke-dasharray:3337;
    stroke-dashoffset:3339;
    stroke-width:128;
    stroke-linecap:round;
    fill:none;
    stroke:#000;
}
svg.acjk path[id] {fill:#ccc;}
]]>
</style>
<path id="z20491d1" d="M244 344C274 304 305 258 337 205C355 172 372 148 388 131C395 123 395 114 392 105C388 96 378 83 358 69C339 55 322 48 309 50C296 50 292 60 299 75C308 97 309 117 302 136C259 239 186 359 79 492C73 499 67 504 66 509C63 516 67 520 77 518C146 475 180 425 219 376C236 355 236 355 244 344Z"/>
<path id="z20491d2" d="M219 376C242 434 244 487 244 544C243 620 233 696 213 773C203 805 210 837 235 870C248 887 266 875 271 864C281 836 285 805 286 774C286 589 289 476 298 435C301 416 301 403 295 396C278 378 264 364 244 344C234 334 214 363 219 376Z"/>
<path id="z20491d3" d="M453 208C446 201 434 195 419 188C411 184 404 183 397 187C388 193 399 224 403 233C420 271 424 401 407 623C404 657 395 695 384 736C372 774 379 809 405 842C412 851 419 852 427 843C435 833 439 821 441 809C442 797 445 785 446 773C451 620 467 400 469 241C469 229 463 215 453 208Z"/>
<path id="z20491d4" d="M792 785C796 814 804 839 826 866C853 900 873 865 883 846C895 818 893 787 891 753C881 641 881 481 876 275C875 252 881 233 893 217C899 208 899 200 893 193C880 178 855 162 817 145C804 138 792 138 781 145C707 173 597 194 453 208C441 210 457 243 469 241C497 235 537 228 591 221C610 218 610 218 620 216C658 208 696 201 735 195C763 190 785 193 799 205C823 227 824 364 821 617C820 652 818 742 804 766C800 772 791 778 792 785Z"/>
<path id="z20491d5" d="M604 358C579 364 552 366 523 372C502 375 498 384 514 392C544 406 576 403 602 398C630 391 630 391 644 388C722 369 762 357 764 356C768 351 769 346 767 342C762 335 751 330 735 327C719 324 691 333 650 345C619 354 619 354 604 358Z"/>
<path id="z20491d6" d="M591 221C589 223 589 228 593 235C610 263 607 286 604 358C603 385 603 385 602 398C600 432 596 468 593 506C593 518 625 510 627 499C631 460 639 423 644 388C648 359 648 359 650 345C654 312 662 280 672 264C678 253 671 245 660 237C645 225 632 219 620 216C610 214 593 211 591 221Z"/>
<path id="z20491d7" d="M548 513C542 510 531 507 517 504C510 502 506 502 504 505C501 507 501 513 505 522C518 552 530 593 537 644C538 658 542 671 551 682C559 693 565 696 569 688C571 681 573 673 573 664C572 654 572 643 570 634C565 611 559 555 559 539C559 530 557 516 548 513Z"/>
<path id="z20491d8" d="M593 506C578 508 563 510 548 513C539 515 550 541 559 539C589 533 620 527 654 522C686 516 688 538 682 558C676 574 670 593 664 615C661 631 704 642 710 627C724 584 740 557 755 545C770 531 770 517 757 508C745 499 731 489 715 478C704 470 692 469 680 476C663 483 647 494 627 499C604 504 604 504 593 506Z"/>
<path id="z20491d9" d="M573 664C585 667 606 661 713 652C719 651 723 648 724 643C724 639 719 633 710 627C696 619 680 615 664 615C641 615 598 629 570 634C560 636 563 662 573 664Z"/>
<path id="z20491d10" d="M441 809C452 811 514 804 792 785C799 785 808 772 804 766C800 759 793 751 780 742C769 735 752 733 731 737C626 754 531 767 446 773C434 773 429 809 441 809Z"/>
<defs>
    <clipPath id="z20491c1"><use xlink:href="#z20491d1"/></clipPath>
    <clipPath id="z20491c2"><use xlink:href="#z20491d2"/></clipPath>
    <clipPath id="z20491c3"><use xlink:href="#z20491d3"/></clipPath>
    <clipPath id="z20491c4"><use xlink:href="#z20491d4"/></clipPath>
    <clipPath id="z20491c5"><use xlink:href="#z20491d5"/></clipPath>
    <clipPath id="z20491c6"><use xlink:href="#z20491d6"/></clipPath>
    <clipPath id="z20491c7"><use xlink:href="#z20491d7"/></clipPath>
    <clipPath id="z20491c8"><use xlink:href="#z20491d8"/></clipPath>
    <clipPath id="z20491c9"><use xlink:href="#z20491d9"/></clipPath>
    <clipPath id="z20491c10"><use xlink:href="#z20491d10"/></clipPath>
</defs>
<path style="--d:1s;" pathLength="3333" clip-path="url(#z20491c1)" d="M303 60L344 94L346 129L212 349L74 510"/>
<path style="--d:2s;" pathLength="3333" clip-path="url(#z20491c2)" d="M238 352L268 427L245 873"/>
<path style="--d:3s;" pathLength="3333" clip-path="url(#z20491c3)" d="M400 193L438 240L442 305L413 844"/>
<path style="--d:4s;" pathLength="3333" clip-path="url(#z20491c4)" d="M456 217L791 168L852 210L835 873"/>
<path style="--d:5s;" pathLength="3333" clip-path="url(#z20491c5)" d="M511 382L763 346"/>
<path style="--d:6s;" pathLength="3333" clip-path="url(#z20491c6)" d="M598 223L640 267L600 505"/>
<path style="--d:7s;" pathLength="3333" clip-path="url(#z20491c7)" d="M510 513L536 538L561 687"/>
<path style="--d:8s;" pathLength="3333" clip-path="url(#z20491c8)" d="M552 519L683 497L715 516L724 539L679 629"/>
<path style="--d:9s;" pathLength="3333" clip-path="url(#z20491c9)" d="M566 646L718 642"/>
<path style="--d:10s;" pathLength="3333" clip-path="url(#z20491c10)" d="M437 798L800 771"/>
</svg>"""
