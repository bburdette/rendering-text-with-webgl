--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Encode
    exposing
        ( axis2d
        , axis3d
        , boundingBox2d
        , boundingBox3d
        , circle2d
        , circle3d
        , cubicSpline2d
        , cubicSpline3d
        , direction2d
        , direction3d
        , ellipse2d
        , frame2d
        , frame3d
        , lineSegment2d
        , lineSegment3d
        , plane3d
        , point2d
        , point3d
        , polygon2d
        , polyline2d
        , polyline3d
        , quadraticSpline2d
        , quadraticSpline3d
        , sketchPlane3d
        , sphere3d
        , triangle2d
        , triangle3d
        , vector2d
        , vector3d
        )

{-| JSON encoders for the core OpenSolid types.

@docs vector2d, vector3d, direction2d, direction3d, point2d, point3d
@docs axis2d, axis3d, plane3d, frame2d, frame3d, sketchPlane3d
@docs lineSegment2d, lineSegment3d, triangle2d, triangle3d
@docs boundingBox2d, boundingBox3d
@docs polyline2d, polyline3d, polygon2d
@docs circle2d, circle3d, ellipse2d, sphere3d
@docs quadraticSpline2d, quadraticSpline3d, cubicSpline2d, cubicSpline3d

-}

import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Json.Encode as Encode exposing (Value)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| Encode a `Vector2d` as an array of two floating-point components.
-}
vector2d : Vector2d -> Value
vector2d vector =
    let
        ( x, y ) =
            Vector2d.components vector
    in
    Encode.list [ Encode.float x, Encode.float y ]


{-| Encode a `Vector3d` as an array of three floating-point components.
-}
vector3d : Vector3d -> Value
vector3d vector =
    let
        ( x, y, z ) =
            Vector3d.components vector
    in
    Encode.list [ Encode.float x, Encode.float y, Encode.float z ]


{-| Encode a `Direction2d` as an array of two floating-point components.
-}
direction2d : Direction2d -> Value
direction2d direction =
    let
        ( x, y ) =
            Direction2d.components direction
    in
    Encode.list [ Encode.float x, Encode.float y ]


{-| Encode a `Direction3d` as an array of three floating-point components.
-}
direction3d : Direction3d -> Value
direction3d direction =
    let
        ( x, y, z ) =
            Direction3d.components direction
    in
    Encode.list [ Encode.float x, Encode.float y, Encode.float z ]


{-| Encode a `Point2d` as an array of two floating-point coordinates.
-}
point2d : Point2d -> Value
point2d point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    Encode.list [ Encode.float x, Encode.float y ]


{-| Encode a `Point3d` as an array of three floating-point coordinates.
-}
point3d : Point3d -> Value
point3d point =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
    Encode.list [ Encode.float x, Encode.float y, Encode.float z ]


{-| Encode an `Axis2d` as an object with `originPoint` and `direction` fields.
-}
axis2d : Axis2d -> Value
axis2d axis =
    Encode.object
        [ ( "originPoint", point2d (Axis2d.originPoint axis) )
        , ( "direction", direction2d (Axis2d.direction axis) )
        ]


{-| Encode an `Axis3d` as an object with `originPoint` and `direction` fields.
-}
axis3d : Axis3d -> Value
axis3d axis =
    Encode.object
        [ ( "originPoint", point3d (Axis3d.originPoint axis) )
        , ( "direction", direction3d (Axis3d.direction axis) )
        ]


{-| Encode a `Plane3d` as an object with `originPoint` and `normalDirection`
fields.
-}
plane3d : Plane3d -> Value
plane3d plane =
    Encode.object
        [ ( "originPoint", point3d (Plane3d.originPoint plane) )
        , ( "normalDirection", direction3d (Plane3d.normalDirection plane) )
        ]


{-| Encode a `Frame2d` as an object with `originPoint`, `xDirection` and
`yDirection` fields.
-}
frame2d : Frame2d -> Value
frame2d frame =
    Encode.object
        [ ( "originPoint", point2d (Frame2d.originPoint frame) )
        , ( "xDirection", direction2d (Frame2d.xDirection frame) )
        , ( "yDirection", direction2d (Frame2d.yDirection frame) )
        ]


{-| Encode a `Frame3d` as an object with `originPoint`, `xDirection`,
`yDirection` and `zDirection` fields.
-}
frame3d : Frame3d -> Value
frame3d frame =
    Encode.object
        [ ( "originPoint", point3d (Frame3d.originPoint frame) )
        , ( "xDirection", direction3d (Frame3d.xDirection frame) )
        , ( "yDirection", direction3d (Frame3d.yDirection frame) )
        , ( "zDirection", direction3d (Frame3d.zDirection frame) )
        ]


{-| Encode a `SketchPlane3d` as an object with `originPoint`, `xDirection` and
`yDirection` fields.
-}
sketchPlane3d : SketchPlane3d -> Value
sketchPlane3d sketchPlane =
    Encode.object
        [ ( "originPoint", point3d (SketchPlane3d.originPoint sketchPlane) )
        , ( "xDirection", direction3d (SketchPlane3d.xDirection sketchPlane) )
        , ( "yDirection", direction3d (SketchPlane3d.yDirection sketchPlane) )
        ]


{-| Encode a `LineSegment2d` as an array of two endpoints.
-}
lineSegment2d : LineSegment2d -> Value
lineSegment2d lineSegment =
    let
        ( startPoint, endPoint ) =
            LineSegment2d.endpoints lineSegment
    in
    Encode.list [ point2d startPoint, point2d endPoint ]


{-| Encode a `LineSegment3d` as an array of two endpoints.
-}
lineSegment3d : LineSegment3d -> Value
lineSegment3d lineSegment =
    let
        ( startPoint, endPoint ) =
            LineSegment3d.endpoints lineSegment
    in
    Encode.list [ point3d startPoint, point3d endPoint ]


{-| Encode a `Triangle2d` as an array of three vertices.
-}
triangle2d : Triangle2d -> Value
triangle2d triangle =
    let
        ( v1, v2, v3 ) =
            Triangle2d.vertices triangle
    in
    Encode.list [ point2d v1, point2d v2, point2d v3 ]


{-| Encode a `Triangle3d` as an array of three vertices.
-}
triangle3d : Triangle3d -> Value
triangle3d triangle =
    let
        ( v1, v2, v3 ) =
            Triangle3d.vertices triangle
    in
    Encode.list [ point3d v1, point3d v2, point3d v3 ]


{-| Encode a `BoundingBox2d` as an object with `minX`, `maxX`, `minY` and `maxY`
fields.
-}
boundingBox2d : BoundingBox2d -> Value
boundingBox2d boundingBox =
    Encode.object
        [ ( "minX", Encode.float (BoundingBox2d.minX boundingBox) )
        , ( "maxX", Encode.float (BoundingBox2d.maxX boundingBox) )
        , ( "minY", Encode.float (BoundingBox2d.minY boundingBox) )
        , ( "maxY", Encode.float (BoundingBox2d.maxY boundingBox) )
        ]


{-| Encode a `BoundingBox3d` as an object with `minX`, `maxX`, `minY`, `maxY`,
`minZ` and `maxZ` fields.
-}
boundingBox3d : BoundingBox3d -> Value
boundingBox3d boundingBox =
    Encode.object
        [ ( "minX", Encode.float (BoundingBox3d.minX boundingBox) )
        , ( "maxX", Encode.float (BoundingBox3d.maxX boundingBox) )
        , ( "minY", Encode.float (BoundingBox3d.minY boundingBox) )
        , ( "maxY", Encode.float (BoundingBox3d.maxY boundingBox) )
        , ( "minZ", Encode.float (BoundingBox3d.minZ boundingBox) )
        , ( "maxZ", Encode.float (BoundingBox3d.maxZ boundingBox) )
        ]


{-| Encode a `Polyline2d` as an array of vertices.
-}
polyline2d : Polyline2d -> Value
polyline2d polyline =
    Encode.list (List.map point2d (Polyline2d.vertices polyline))


{-| Encode a `Polyline3d` as an array of vertices.
-}
polyline3d : Polyline3d -> Value
polyline3d polyline =
    Encode.list (List.map point3d (Polyline3d.vertices polyline))


{-| Encode a `Polygon2d` as an object with:

  - an `outerLoop` field which is an array of encoded `Point2d` values
  - an `innerLoops` field which is an array of arrays of encoded `Point2d`
    values

-}
polygon2d : Polygon2d -> Value
polygon2d polygon =
    let
        encodeLoop vertices =
            Encode.list (List.map point2d vertices)

        outerLoop =
            Polygon2d.outerLoop polygon

        innerLoops =
            Polygon2d.innerLoops polygon
    in
    Encode.object
        [ ( "outerLoop", encodeLoop outerLoop )
        , ( "innerLoops", Encode.list (List.map encodeLoop innerLoops) )
        ]


{-| Encode a `Circle2d` as an object with `centerPoint` and `radius` fields.
-}
circle2d : Circle2d -> Value
circle2d circle =
    Encode.object
        [ ( "centerPoint", point2d (Circle2d.centerPoint circle) )
        , ( "radius", Encode.float (Circle2d.radius circle) )
        ]


{-| Encode a `Circle3d` as an object with `centerPoint`, `axialDirection` and
`radius` fields.
-}
circle3d : Circle3d -> Value
circle3d circle =
    Encode.object
        [ ( "centerPoint", point3d (Circle3d.centerPoint circle) )
        , ( "axialDirection", direction3d (Circle3d.axialDirection circle) )
        , ( "radius", Encode.float (Circle3d.radius circle) )
        ]


{-| Encode an `Ellipse2d` as an object with `centerPoint`, `xDirection`,
`xRadius` and `yRadius` fields.
-}
ellipse2d : Ellipse2d -> Value
ellipse2d ellipse =
    Encode.object
        [ ( "centerPoint", point2d (Ellipse2d.centerPoint ellipse) )
        , ( "xDirection", direction2d (Ellipse2d.xDirection ellipse) )
        , ( "xRadius", Encode.float (Ellipse2d.xRadius ellipse) )
        , ( "yRadius", Encode.float (Ellipse2d.yRadius ellipse) )
        ]


{-| Encode a `Sphere` as an object with `centerPoint` and `radius` fields.
-}
sphere3d : Sphere3d -> Value
sphere3d sphere =
    Encode.object
        [ ( "centerPoint", point3d (Sphere3d.centerPoint sphere) )
        , ( "radius", Encode.float (Sphere3d.radius sphere) )
        ]


{-| Encode a `QuadraticSpline2d` as an array of three control points.
-}
quadraticSpline2d : QuadraticSpline2d -> Value
quadraticSpline2d spline =
    Encode.list
        [ point2d (QuadraticSpline2d.startPoint spline)
        , point2d (QuadraticSpline2d.controlPoint spline)
        , point2d (QuadraticSpline2d.endPoint spline)
        ]


{-| Encode a `QuadraticSpline3d` as an array of three control points.
-}
quadraticSpline3d : QuadraticSpline3d -> Value
quadraticSpline3d spline =
    Encode.list
        [ point3d (QuadraticSpline3d.startPoint spline)
        , point3d (QuadraticSpline3d.controlPoint spline)
        , point3d (QuadraticSpline3d.endPoint spline)
        ]


{-| Encode a `CubicSpline2d` as an array of four control points.
-}
cubicSpline2d : CubicSpline2d -> Value
cubicSpline2d spline =
    Encode.list
        [ point2d (CubicSpline2d.startPoint spline)
        , point2d (CubicSpline2d.startControlPoint spline)
        , point2d (CubicSpline2d.endControlPoint spline)
        , point2d (CubicSpline2d.endPoint spline)
        ]


{-| Encode a `CubicSpline3d` as an array of four control points.
-}
cubicSpline3d : CubicSpline3d -> Value
cubicSpline3d spline =
    Encode.list
        [ point3d (CubicSpline3d.startPoint spline)
        , point3d (CubicSpline3d.startControlPoint spline)
        , point3d (CubicSpline3d.endControlPoint spline)
        , point3d (CubicSpline3d.endPoint spline)
        ]