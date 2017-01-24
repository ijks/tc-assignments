{-# LANGUAGE RecordWildCards #-}

module CSharpAlgebra where

import CSharpLex
import CSharpGram


data CSharpAlgebra clas memb stat expr = CSharpA
    { classDecl :: Token -> [memb] -> clas
    , memberDecl :: MemberAlgebra memb stat
    , statement :: StatAlgebra expr stat
    , expression :: ExprAlgebra expr
    }

data MemberAlgebra memb stat = MemberA
    { memberD :: Decl -> memb
    , memberM :: Type -> Token -> [Decl] -> stat -> memb
    }

data StatAlgebra expr stat = StatA
    { statDecl :: Decl -> stat
    , statExpr :: expr -> stat
    , statIf :: expr -> stat -> stat -> stat
    , statWhile :: expr -> stat -> stat
    , statReturn :: expr -> stat
    , statBlock :: [stat] -> stat
    }

data ExprAlgebra expr = ExprA
    { exprConst :: Token -> expr
    , exprVar :: Token -> expr
    , exprOper :: Token -> expr -> expr -> expr
    , exprCall :: Token -> [expr] -> expr
    }


foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp CSharpA { .. } = fClas
    where
        (MemberA { .. }, StatA { .. }, ExprA { .. })
            = (memberDecl, statement, expression)

        fClas (Class c ms) = classDecl c (map fMemb ms)

        fMemb (MemberD d)        = memberD d
        fMemb (MemberM t m ps s) = memberM t m ps (fStat s)

        fStat (StatDecl d)     = statDecl d
        fStat (StatExpr e)     = statExpr (fExpr e)
        fStat (StatIf e s1 s2) = statIf (fExpr e) (fStat s1) (fStat s2)
        fStat (StatWhile e s1) = statWhile (fExpr e) (fStat s1)
        fStat (StatReturn e)   = statReturn (fExpr e)
        fStat (StatBlock ss)   = statBlock (map fStat ss)

        fExpr (ExprConst con)     = exprConst con
        fExpr (ExprVar var)       = exprVar var
        fExpr (ExprOper op e1 e2) = exprOper op (fExpr e1) (fExpr e2)
        fExpr (ExprCall fn args)  = exprCall fn (map fExpr args)
