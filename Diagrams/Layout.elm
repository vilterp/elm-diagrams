module Diagrams.Layout where

type alias Row a = List (RowElem a)
type RowElem a = DiagE (Diagram a)
               | SpringE

layout : List (Row a) -> Diagram a
{-
for each row:
get a min & max width
-}

type Constraint = Value Float
                | NegInfinity
                | PosInfinity

type alias ConstraintInterval = (Constraint, Constraint) -- TODO closed & open

{-
then fold over that list & get a value that works for everyone
return Result ConstraintError Float
then render each row with that width into a diagram
check <= 1 contracting spring; else ambiguous (?)
-}
renderRow : Float -> Row a -> Diagram a
-- turn ContractingSprings into spaces
