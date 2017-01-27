module CSharpPrettyPrint where

import Data.List (intercalate)
import CSharpLex
import CSharpGram
import CSharpAlgebra


ppAlgebra :: CSharpAlgebra [String] [String] [String] [String]
ppAlgebra = CSharpA
    { classDecl = ppClass
    , memberDecl = MemberA 
        { memberD = ppDecl
        , memberM = ppMembMeth 
        }
    , statement = StatA
        { statDecl = map (++";") . ppDecl 
        , statExpr = ppStatExpr
        , statIf = ppStatIf
        , statWhile = ppStatWhile
        , statReturn = ppStatReturn
        , statBlock = ppBlock
        }
    , expression = ExprA
        { exprConst = ppExprCon
        , exprVar = ppExprVar
        , exprOper = ppExprOp
        , exprCall = ppExprCall
        }
    }



ppClass :: Token -> [[String]] -> [String]
ppClass (UpperId name) body = 
    ["class " ++ name] ++ ppBlock body

ppDecl :: Decl -> [String]
ppDecl (Decl typ (LowerId name)) =
    [ppType typ ++ ' ' : name]

ppMembMeth :: Type -> Token -> [Decl] -> [String] -> [String]
ppMembMeth typ (LowerId name) decs body =
    [ppType typ ++ ' ' : name ++ ppParentheses (ppList $ concat (ppDecl <$> decs))] ++ body

ppStatExpr :: [String] -> [String]
ppStatExpr = map (++";")

ppStatIf :: [String] -> [String] -> [String] -> [String]
ppStatIf [condition] true false =
    ["if " ++ ppParentheses condition] ++ true
        ++ if length false <= 2 then [] else (["else"] ++ false)

ppStatWhile :: [String] -> [String] -> [String]
ppStatWhile [condition] body = 
    ["while " ++ ppParentheses condition] ++ body

ppStatReturn :: [String] -> [String]
ppStatReturn [ret] = ["return " ++ ret ++ ";"]

ppBlock :: [[String]] -> [String]
ppBlock body = ["{"] ++ concat (indented <$> body) ++ ["}"]

ppExprCon :: Token -> [String]
ppExprCon (ConstBool True) = ["true"]
ppExprCon (ConstBool False) = ["false"]
ppExprCon (ConstChar c) = [show c]
ppExprCon (ConstInt n) = [show n]

ppExprVar :: Token -> [String]
ppExprVar (LowerId name) = [name]

ppExprOp :: Token -> [String] -> [String] -> [String]
ppExprOp (Operator op) [lhs] [rhs] = [intercalate " " [lhs, op, rhs]]

ppExprCall :: Token -> [[String]] -> [String] 
ppExprCall (LowerId name) params = name : (ppParentheses <$> [ppList (concat params)])


indented :: [String] -> [String]
indented = map ("    "++)

ppParentheses :: String -> String
ppParentheses s = "(" ++ s ++ ")"

ppList :: [String] -> String
ppList = (intercalate ", ")

ppType :: Type -> String
ppType TypeVoid = "void"
ppType (TypePrim (StdType name)) = name
ppType (TypeObj (UpperId name)) = name
ppType (TypeArray typ) = ppType typ ++ "[]"