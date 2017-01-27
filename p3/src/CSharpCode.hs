module CSharpCode where

import Prelude hiding (LT, GT, EQ)

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Char (ord)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Data.Map as M hiding (foldl, foldr, mapMaybe)

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

-- REMOVE ME --
import Debug.Trace

data ValueOrAddress = Value | Address
    deriving Show

type Address = Int
type Signature = (Type, [Decl])

type LocalEnv = Map String Address
type ClassEnv = Map String Signature

-- We use the 'State' monad to keep track of the environment of an
-- expression, to prevent having to pass around extra parameters all
-- the time.
type Env = State (LocalEnv, ClassEnv) Code

-- Ignore the resulting state from a computation
ignore :: State s a -> State s a
ignore st = do
    s <- get
    return $ evalState st s

-- Run one 'Env's after the other, appending the resulting code.
chainEnv :: Env -> Env -> Env
chainEnv env stat = do
    code <- env
    code' <- stat
    return $ code ++ code'

-- We decided to write this as a function, because that's much more
-- succinct than defining an algebra. Using the 'circular program'
-- method outlined in the assignment also did not seem to work, instead
-- causing the runtime to print a cryptic '<<loop>>' error.
funcDecls :: Class -> ClassEnv
funcDecls (Class _ members) = fromList $ mapMaybe go members
    where
        go (MemberD decl) =
            Nothing
        go (MemberM ty (LowerId ident) args _) =
            Just (ident, (ty, args))


codeAlgebra :: CSharpAlgebra (ClassEnv -> Code) (ClassEnv -> Code) Env (ValueOrAddress -> Env)
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

fClas :: Token -> [ClassEnv -> Code] -> ClassEnv -> Code
fClas ident members cenv =
    let code = members >>= ($ cenv)
    in [Bsr "main", HALT] ++ code

fMembDecl :: Decl -> ClassEnv -> Code
fMembDecl d e = []

fMembMeth :: Type -> Token -> [Decl] -> Env -> ClassEnv -> Code
fMembMeth ty (LowerId ident) args stats cenv =
    let
        (code, (lenv, _)) = runState stats (argsEnv args, cenv)
        localVars = M.size $ M.filter (> 0) lenv
    in
        [LABEL ident, LINK localVars] ++ code ++ [UNLINK, RET]

argsEnv :: [Decl] -> LocalEnv
argsEnv args = fromList $ zip (getIdent <$> args) [-2, -3..]
    where getIdent (Decl _ (LowerId ident)) = ident

fStatDecl :: Decl -> Env
fStatDecl (Decl _ (LowerId ident)) = do
    defs <- gets fst
    let loc = foldr max 0 defs + 1 -- the highest allocated address, plus 1
    modify $ first (M.insert ident loc)
    return [LDC 0, LDLA loc, STA 0]

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
    (++ [STR r3]) <$> expr Value

fStatBlock :: [Env] -> Env
fStatBlock stats =
    ignore $ foldl chainEnv (return []) stats

fExprCon :: Token -> ValueOrAddress -> Env
fExprCon (ConstInt n) va = return [LDC n]
fExprCon (ConstBool False) va = return [LDC 0]
fExprCon (ConstBool True) va = return [LDC 1]
fExprCon (ConstChar c) va = return [LDC (ord c)]


fExprVar :: Token -> ValueOrAddress -> Env
fExprVar (LowerId ident) va = do
    defs <- gets fst
    let
        loc = case M.lookup ident defs of
            Just l -> l
            Nothing -> error $ "Undefined variable: '" ++ ident ++ "'"
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
fExprCall (LowerId ident) args va = do
    env <- get
    let code = fmap (\f -> evalState (f va) env) args
    if ident == "print" then
        return $ printCall code
    else do
        let sig = snd env ! ident
        return $ funcCall ident sig code

printCall :: [Code] -> Code
printCall c = intercalate [LDS 0, TRAP 0] c ++ [LDS 0, TRAP 0]

funcCall :: String -> Signature -> [Code] -> Code
funcCall ident (ty, args) code = concat
    [ concat code
    , [Bsr ident]
    , case ty of
        TypePrim _ -> [LDR r3]
        _ -> []
    ]

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
