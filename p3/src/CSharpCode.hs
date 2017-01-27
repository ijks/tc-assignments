module CSharpCode where

import Prelude hiding (LT, GT, EQ)

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Char
import Data.List (intercalate)
import Data.Map as M hiding (foldl, foldr)

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type Address = Int

type LocalEnv = Map String Address
type ClassEnv = Map String Member

type Env = State (LocalEnv, ClassEnv) Code

-- Helper function so we can ignore the modified environment from
-- nested blocks.
ignore :: State s a -> State s a
ignore st = do
    s <- get
    return $ evalState st s

codeAlgebra :: CSharpAlgebra Code Code Env (ValueOrAddress -> Env)
codeAlgebra = CSharpA
    { classDecl = fClas
    , memberDecl = MemberA { memberD = fMembDecl, memberM = fMembMeth }
    , statement = StatA
        { statDecl = fStatDecl
        , statExpr = fStatExpr
        , statIf = fStatIf
        , statWhile = fStatWhile
        , statReturn = fStatReturn
        , statBlock = fStatBlock
        }
    , expression = ExprA
        { exprConst = fExprCon
        , exprVar = fExprVar
        , exprOper = fExprOp
        , exprCall = fExprCall
        }
    }

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> Env -> Code
fMembMeth t (LowerId x) args stats =
    let
        (code, (lenv, _)) = runState stats (M.empty, M.empty)
        localVars = M.size lenv
    in [LABEL x, LINK localVars] ++ code ++ [UNLINK, RET]

fStatDecl :: Decl -> Env
fStatDecl (Decl _ (LowerId ident)) = do
    let loc = foldr max 0 env + 1 -- the highest allocated address, plus 1
    modify $ first (M.insert ident loc)
    return []

fStatExpr :: (ValueOrAddress -> Env) -> Env
fStatExpr expr = expr Value

fStatIf :: (ValueOrAddress -> Env) -> Env -> Env -> Env
fStatIf expr true false = do
    ct <- ignore true
    cf <- ignore false
    ce <- expr Value
    let (nt, nf) = (codeSize ct, codeSize cf)
    return $ ce ++ [BRF (nt + 2)] ++ ct ++ [BRA nf] ++ cf

fStatWhile :: (ValueOrAddress -> Env) -> Env -> Env
fStatWhile expr body = do
    cb <- ignore body
    ce <- expr Value
    let (n, k) = (codeSize cb, codeSize ce)
    return $ [BRA n] ++ cb ++ ce ++ [BRT (-(n + k + 2))]

fStatReturn :: (ValueOrAddress -> Env) -> Env
fStatReturn expr =
    (++ [pop, RET]) <$> expr Value

fStatBlock :: [Env] -> Env
fStatBlock stats =
    foldl combine (return []) stats
    where
        combine env stat = do
            code <- env
            code' <- stat
            return $ code ++ code'

fExprCon :: Token -> ValueOrAddress -> Env
fExprCon (ConstInt n) va = return [LDC n]
fExprCon (ConstBool False) va = return [LDC 0]
fExprCon (ConstBool True) va = return [LDC 1]
fExprCon (ConstChar c) va = return [LDC (ord c)]


fExprVar :: Token -> ValueOrAddress -> Env
fExprVar (LowerId ident) va = do
    defs <- gets fst
    let loc = defs ! ident
    return $ case va of
        Value    ->  [LDL  loc]
        Address  ->  [LDLA loc]

fExprOp :: Token
    -> (ValueOrAddress -> Env)
    -> (ValueOrAddress -> Env)
    -> ValueOrAddress -> Env
fExprOp (Operator "=") lhs rhs va = do
    rc <- rhs Value
    lc <- lhs Address
    return $ rc ++ [LDS 0] ++ lc ++ [STA 0]
fExprOp (Operator op)  lhs rhs va = do
    lc <- lhs Value
    rc <- rhs Value
    return $ lc ++ rc ++ [opCodes ! op]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

fExprCall :: Token
    -> [ValueOrAddress -> Env]
    -> ValueOrAddress -> Env
fExprCall (LowerId ident) args va =
    if ident == "print" then
        return $ intercalate [TRAP 0] code ++ [TRAP 0]
    else
        return $ concat code ++ [Bsr ident]
    where
        code = fmap (\f -> evalState $ f va) args
fExprCall (LowerId ident) args va =
    (args >>= (\f -> evalState $ f va)) ++ [Bsr ident]


-- Sugar TODO: move to better place

desugarAlgebra :: CSharpAlgebra Class Member Stat Expr
desugarAlgebra = CSharpA
    { classDecl = Class
    , memberDecl = MemberA
        { memberD = MemberD
        , memberM = MemberM
        }
    , statement = StatA
        { statDecl = StatDecl
        , statExpr = StatExpr
        , statIf = StatIf
        , statWhile = StatWhile
        , statReturn = StatReturn
        , statBlock = StatBlock
        }
    , expression = ExprA
        { exprConst = ExprConst
        , exprVar = ExprVar
        , exprOper = desugarOps
        , exprCall = ExprCall
        }
    }

desugarOps :: Token -> Expr -> Expr -> Expr
desugarOps (Operator op) lhs rhs =
    case M.lookup op sugarOps of
        Just o ->
            ExprOper (Operator "=") lhs (ExprOper (Operator o) lhs rhs)
        Nothing ->
            ExprOper (Operator op) lhs rhs
    where
        sugarOps = fromList
            [ ("+=", "+")
            , ("-=", "-")
            , ("*=", "*")
            , ("/=", "/")
            , ("%=", "%")
            , ("|=", "||")
            , ("&=", "&&")
            , ("^=", "^")
            ]
