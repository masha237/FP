module HW3.Pretty (prettyValue) where

import HW3.Base
import Prettyprinter (Doc, Pretty (pretty))
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction HiFunAdd) = pretty "add"
prettyValue (HiValueFunction HiFunMul) = pretty "mul"
prettyValue (HiValueFunction HiFunDiv) = pretty "div"
prettyValue (HiValueFunction HiFunSub) = pretty "sub"
prettyValue (HiValueFunction HiFunNot) = pretty "not"
prettyValue (HiValueFunction HiFunAnd) = pretty "and"
prettyValue (HiValueFunction HiFunOr) = pretty "or"
prettyValue (HiValueFunction HiFunLessThan) = pretty "less-than"
prettyValue (HiValueFunction HiFunGreaterThan) = pretty "greater-than"
prettyValue (HiValueFunction HiFunEquals) = pretty "equals"
prettyValue (HiValueFunction HiFunNotLessThan) = pretty "not-less-than"
prettyValue (HiValueFunction HiFunNotGreaterThan) = pretty "not-greater-than"
prettyValue (HiValueFunction HiFunNotEquals) = pretty "not-equals"
prettyValue (HiValueFunction HiFunIf) = pretty "if"

prettyValue (HiValueNumber _) = pretty "ochen' zhal' i lose"

prettyValue (HiValueBool x) = pretty (if x then "true" else "false")

