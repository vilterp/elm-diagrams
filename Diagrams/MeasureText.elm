module Diagrams.MeasureText where

{-| Use canvas for measuring text -}

import Text
import Native.Diagrams.MeasureText

textWidth : Text.Text -> Float
textWidth = Native.Diagrams.MeasureText.textWidth
