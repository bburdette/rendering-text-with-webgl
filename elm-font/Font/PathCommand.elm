module Font.PathCommand
    exposing
        ( PathCommand(..)
        , pathToPolygon
        , triangulate
        , winding
        )

import Array.Hamt as Array
import CubicSpline2d exposing (CubicSpline2d)
import Point2d exposing (Point2d)
import Polygon2d as Polygon2d exposing (Polygon2d)
import Polygon2d.Monotone as Monotone
import QuadraticSpline2d exposing (QuadraticSpline2d)


type PathCommand
    = MoveTo Float Float
    | LineTo Float Float
    | QuadraticCurveTo Float Float Float Float
    | BezierCurveTo Float Float Float Float Float Float


{-| True for counter-clock-wise
-}
winding : List Point2d -> Bool
winding points =
    case points of
        first :: _ :: rest ->
            windingHelp first points 0 < 0

        _ ->
            False


windingHelp : Point2d -> List Point2d -> Float -> Float
windingHelp first points w =
    case points of
        p1 :: p2 :: rest ->
            let
                ( x1, y1 ) =
                    Point2d.coordinates p1

                ( x2, y2 ) =
                    Point2d.coordinates p2
            in
            windingHelp first (p2 :: rest) (w + (x2 - x1) * (y2 + y1))

        p1 :: [] ->
            let
                ( x1, y1 ) =
                    Point2d.coordinates p1

                ( x2, y2 ) =
                    Point2d.coordinates first
            in
            w + (x2 - x1) * (y2 + y1)

        [] ->
            w


pointInPath : List Point2d -> Point2d -> Bool
pointInPath startPath point =
    let
        ( px, py ) =
            Point2d.coordinates point

        checkFor ( pjx, pjy ) ( pix, piy ) result =
            if ((piy > py) /= (pjy > py)) && (px < (pjx - pix) * (py - piy) / (pjy - piy) + pix) then
                not result

            else
                result

        pointInPathHelp veryFirst path result =
            case path of
                first :: second :: rest ->
                    pointInPathHelp
                        veryFirst
                        (second :: rest)
                        (checkFor (Point2d.coordinates first) (Point2d.coordinates second) result)

                last :: [] ->
                    checkFor (Point2d.coordinates last) (Point2d.coordinates veryFirst) result

                [] ->
                    result
    in
    case startPath of
        first :: second :: rest ->
            pointInPathHelp first startPath False

        _ ->
            False


triangulate : List (List Point2d) -> List ( Point2d, Point2d, Point2d )
triangulate path =
    let
        ( outlines, holes ) =
            List.partition winding path
    in
    outlines
        |> List.map
            (\outerLoop ->
                Polygon2d.with
                    { outerLoop = outerLoop
                    , innerLoops =
                        holes
                            |> List.filter
                                (List.head
                                    >> Maybe.map (pointInPath outerLoop)
                                    >> Maybe.withDefault False
                                )
                    }
                    |> Monotone.monotonePolygons
            )
        |> List.concatMap
            (\( points, loops ) ->
                List.concatMap
                    (\vertices ->
                        vertices
                            |> Monotone.faces
                            |> List.filterMap
                                (\( i, j, k ) ->
                                    Maybe.map3
                                        (,,)
                                        (Array.get i points)
                                        (Array.get j points)
                                        (Array.get k points)
                                )
                    )
                    loops
            )


pathToPolygon : List PathCommand -> List Point2d
pathToPolygon commands =
    Monotone.removeDuplicates (List.reverse (pathToPolygonHelp commands []))


pathToPolygonHelp : List PathCommand -> List Point2d -> List Point2d
pathToPolygonHelp commands loop =
    case commands of
        [] ->
            loop

        (MoveTo x y) :: remaining ->
            pathToPolygonHelp
                remaining
                (Point2d.fromCoordinates ( x, y ) :: loop)

        (LineTo x y) :: remaining ->
            pathToPolygonHelp
                remaining
                (Point2d.fromCoordinates ( x, y ) :: loop)

        (QuadraticCurveTo x1 y1 x2 y2) :: remaining ->
            pathToPolygonHelp
                remaining
                (case loop of
                    [] ->
                        loop

                    point :: rest ->
                        QuadraticSpline2d.pointsOn
                            (QuadraticSpline2d.with
                                { startPoint = Point2d.fromCoordinates ( x2, y2 )
                                , controlPoint = Point2d.fromCoordinates ( x1, y1 )
                                , endPoint = point
                                }
                            )
                            (samples 10)
                            ++ rest
                )

        (BezierCurveTo x1 y1 x2 y2 x3 y3) :: remaining ->
            pathToPolygonHelp
                remaining
                (case loop of
                    [] ->
                        loop

                    point :: rest ->
                        CubicSpline2d.pointsOn
                            (CubicSpline2d.with
                                { startPoint = Point2d.fromCoordinates ( x3, y3 )
                                , startControlPoint = Point2d.fromCoordinates ( x2, y2 )
                                , endControlPoint = Point2d.fromCoordinates ( x1, y1 )
                                , endPoint = point
                                }
                            )
                            (samples 10)
                            ++ rest
                )


samples : Int -> List Float
samples n =
    List.map
        (\a -> toFloat a / toFloat n)
        (List.range 0 n)
