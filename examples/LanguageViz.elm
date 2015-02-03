module LanguageViz where

import Diagrams (..)
import Window
import Mouse
import Signal as S
import List as L
import Text as T
import Color as C

-- EXPR MODEL

type Expr = Ap Expr (List Expr)
          | IfExpr Expr Expr Expr
          | Variable String
          | LetExpr (List (String, Expr)) Expr
          -- literals
          | DataConstructor -- ...
          | IntLit Int
          | StringLit String
          | FloatLit Float

-- STYLES

defTextStyle = let ds = T.defaultStyle
               in { ds | typeface <- ["Courier", "Courier New", "Lucida Console",
                                      "Monaco", "Consolas"] }
numberLitStyle = { defTextStyle | color <- C.blue }
intLitStyle = numberLitStyle
floatLitStyle = numberLitStyle
stringLitStyle = { defTextStyle | color <- C.green }
varStyle = defTextStyle

keywordStyle = { defTextStyle | color <- C.orange
                              , bold <- True }

hSpacer = hspace 7

keyword : String -> Diagram a
keyword kw = text kw keywordStyle

varDia : String -> Diagram a
varDia v = text v varStyle

-- VIEW

view : Expr -> Diagram a
view expr =
  case expr of
    IntLit x -> text (toString x) intLitStyle
    FloatLit x -> text (toString x) floatLitStyle
    StringLit s -> text ("\"" ++ s ++ "\"") stringLitStyle
    Variable name -> varDia name
    Ap func args -> let comma = text "," defTextStyle
                        argsViews = L.map view args
                        allArgs = hcat <| L.intersperse comma argsViews
                    in hcat <| [view func,
                                  text "(" defTextStyle,
                                  allArgs,
                                  text ")" defTextStyle]
    IfExpr cond tbranch fbranch ->
        alignLeft [ hcat [keyword "if", hSpacer, view cond]
                    , hcat [keyword "then", hSpacer, view tbranch]
                    , hcat [keyword "else", hSpacer, view fbranch]
                    ]
    LetExpr bindings expr -> let eq = keyword "="
                                 binding (name, exp) = hcat [varDia name, hSpacer, eq, hSpacer, view exp]
                                 bindingDias = L.map binding bindings
                             in alignLeft [ hcat [keyword "let", hSpacer, vcat bindingDias]
                                            , hcat [keyword "in", hSpacer, view expr] ]

-- TEST DATA

expr = LetExpr [ ("foo", IntLit 62)
               , ("bar", StringLit "Elm is cool")
               , ("baz", IntLit 57) ]
               (IfExpr
                  (Ap (Variable "foo") [(IntLit 2), (Variable "x")])
                  (StringLit "yes")
                  (StringLit "no"))

-- INVOCATION

dia = view expr

main = fullWindowMain dia
