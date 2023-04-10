module HW3.Parser (parse) where

import HW3.Base
    ( HiFun(..),
      HiValue(HiValueNumber, HiValueFunction),
      HiExpr(HiExprValue, HiExprApply) )
import Text.Megaparsec.Error ( ParseErrorBundle )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Text.Megaparsec (Parsec, eof, runParser, between, ParsecT, Stream (Tokens), many)
import Text.Megaparsec.Char ( space, string )
import Control.Applicative.Combinators (choice, sepBy)
import Text.Megaparsec.Char.Lexer (scientific)
import Control.Monad.Combinators.Expr ( Operator (..), makeExprParser )
import Data.Functor.Identity ( Identity )


fabrica :: HiFun -> HiExpr -> HiExpr -> HiExpr
fabrica f a b = HiExprApply (HiExprValue (HiValueFunction f)) [a, b]


table :: [[Operator HiParser HiExpr]]
table = [ 
    [
      Postfix manyPostfix 
    ]
  , [ binaryL "*" (fabrica HiFunMul)
    , binaryL "/" (fabrica HiFunDiv)
    ]
  , [ binaryL "+" (fabrica HiFunAdd)
    , binaryL "-" (fabrica HiFunSub)
    ]
  , [
      binaryN "==" (fabrica HiFunEquals)
    , binaryN "/=" (fabrica HiFunNotEquals)
    , binaryN "<" (fabrica HiFunLessThan)
    , binaryN "<=" (fabrica HiFunNotGreaterThan)
    , binaryN ">" (fabrica HiFunGreaterThan)
    , binaryN ">=" (fabrica HiFunNotLessThan)
    ]  
  , [
      binaryR "&&" (fabrica HiFunAnd)
    ] 
  , [
      binaryR "||" (fabrica HiFunOr)
    ]
  ]

manyPostfix :: HiParser (HiExpr -> HiExpr)
manyPostfix = do
              args <- many pHiPostfix
              return (\val -> foldl HiExprApply val args)

pHiPostfix :: HiParser [HiExpr]
pHiPostfix = parens pHiArgs           

binaryL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr
binaryL str f = InfixL (f <$ string str <* space);

binaryN :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr
binaryN str f = InfixN (f <$ string str <* space);


binaryR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator HiParser HiExpr
binaryR str f = InfixR (f <$ string str <* space);


parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> makeHiParser <* space <* eof) ""

makeHiParser :: HiParser HiExpr
makeHiParser = space *> makeExprParser pExpr table <* space 

pExpr :: HiParser HiExpr
pExpr =  space *> choice [pHiValue, pHiFun, parens makeHiParser] <* space 


pHiArgs :: HiParser [HiExpr]
pHiArgs = space *> sepBy makeHiParser (lexeme (string ",")) <* space
          


pHiFun :: HiParser HiExpr
pHiFun = HiExprValue . HiValueFunction <$> lexeme (choice
  [ HiFunAdd <$ string "add"
  , HiFunMul <$ string "mul"
  , HiFunSub <$ string "sub"
  , HiFunDiv <$ string "div"
  , HiFunNot <$ string "not"
  , HiFunAnd <$ string "and"
  , HiFunOr <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , HiFunNotLessThan <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals <$ string "not-equals"
  , HiFunIf <$ string "if"
  ])

parens :: HiParser a -> HiParser a
parens = between (symbol "(") (symbol ")")

-- brack :: HiParser a -> HiParser a
-- brack = between (symbol "[") (symbol "]")

symbol :: Tokens [Char] -> ParsecT Void String Identity (Tokens [Char])
symbol = L.symbol space


pHiValue :: HiParser HiExpr
pHiValue = HiExprValue <$> (space *> lexeme (HiValueNumber . toRational <$> L.signed space scientific) <* space)
  


type HiParser = Parsec Void String


lexeme :: HiParser a -> HiParser a
lexeme = L.lexeme space

