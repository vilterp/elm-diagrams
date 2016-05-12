module Diagrams.Geom exposing (..)

{-| Geometry utilities used by the rest of the library.

# Transforms
@docs Transform, applyTrans, invertTrans

# Points
@docs Point, magnitude, pointAdd, pointNegate, pointSubtract

# Boxes
@docs BBox, OffsetDimsBox, Dims, bbox2offsetDims, pointInside

# Other
@docs lerp, Direction, directionAngle
-}

{-|-}
type Direction = Up | Down | Left | Right

{-| Given a direction, return an angle in radians. -}
directionAngle : Direction -> Float
directionAngle dir =
  case dir of
    Right -> 0
    Up -> (pi)/2
    Left -> pi
    Down -> (3*pi)/2

-- Transforms

{-|-}
type Transform
  = Translate Float Float
  | Rotate Float
  | Scale Float

{-|-}
applyTrans : Transform -> Point -> Point
applyTrans trans (x, y) = 
  case trans of
    Scale s -> (x*s, y*s)
    Rotate angle -> let c = cos angle
                        s = sin angle
                    in (c*x - s*y, s*x + c*y)
    Translate tx ty -> (x + tx, y + ty)

{-|-}
invertTrans : Transform -> Transform
invertTrans t =
  case t of
    Rotate angle -> Rotate (-angle)
    Scale factor -> Scale (1/factor)
    Translate x y -> Translate (-x) (-y)

-- Points

{-|-}
type alias Point =
  (Float, Float)

{-|-}
magnitude : Point -> Float
magnitude (x, y) = sqrt <| (x^2) + (y^2)

-- TODO: vec vs point (issue #15)
{-|-}
pointAdd : Point -> Point -> Point
pointAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-|-}
pointNegate : Point -> Point
pointNegate (x, y) = (-x, -y)

{-|-}
pointSubtract : Point -> Point -> Point
pointSubtract a b = a `pointAdd` (pointNegate b)

-- Boxes

{-| Bounding box -}
type alias BBox =
  { up : Float
  , down : Float
  , left : Float
  , right : Float
  }

-- offset is the middle of the box
{-| A box defined by its offset and dimensions -}
type alias OffsetDimsBox =
  { offset : (Float, Float)
  , dims : Dims
  } -- TODO: translate is a vector (?)

{-| Dimensions -}
type alias Dims =
  { width : Float
  , height : Float
  }

{-|-}
bbox2offsetDims : BBox -> OffsetDimsBox
bbox2offsetDims bbox =
  { offset = ((bbox.right - bbox.left)/2, (bbox.up - bbox.down)/2)
  , dims = { width = bbox.right + bbox.left
           , height = bbox.up + bbox.down
           }
  }

{-|-}
pointInside : Point -> OffsetDimsBox -> Bool
pointInside (x, y) {offset, dims} =
  let (ox, oy) = offset
      halfWidth = dims.width/2
      halfHeight = dims.height/2
  in (x >= ox - halfWidth && x <= ox + halfWidth) &&
       (y >= oy - halfHeight && y <= oy + halfHeight)

-- Other

{-| linear interpolation. To map x from interval (imin, imax) to (omin, omax), use:

    lerp (omin, omax) (imin, imax) x

-}
lerp : (Float, Float) -> (Float, Float) -> Float -> Float
lerp (omin, omax) (imin, imax) input =
  omin + (omax - omin) * (input - imin) / (imax - imin)
