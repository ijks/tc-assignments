module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Char
import Data.Map as M hiding (foldl, foldr)
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type Env = Map String Int

-- TODO: maybe use @State Env Code@ instead?
type CodeEnv = Env -> (Code, Env)


codeAlgebra :: CSharpAlgebra Code Code CodeEnv (ValueOrAddress -> Env -> Code)
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

fMembMeth :: Type -> Token -> [Decl] -> CodeEnv -> Code
fMembMeth t (LowerId x) args stats =
    let
        (code, env) = stats $ M.empty
        localVars = M.size env
    in [LABEL x, LINK localVars] ++ code ++ [UNLINK, RET]

fStatDecl :: Decl -> CodeEnv
fStatDecl (Decl _ (LowerId ident)) env =
    let loc = foldr (max) 0 env + 1 -- the highest allocated address, plus 1
    in ([], M.insert ident loc env)

fStatExpr :: (ValueOrAddress -> Env -> Code) -> CodeEnv
fStatExpr expr = \env -> (expr Value env ++ [pop], env)

fStatIf :: (ValueOrAddress -> Env -> Code) -> CodeEnv -> CodeEnv -> CodeEnv
fStatIf expr t f = \env ->
    let
        (ct, _) = t env
        (cf, _) = f env
        (nt, nf) = (codeSize ct, codeSize cf)
    in
        (expr Value env ++ [BRF (nt + 2)] ++ ct ++ [BRA nf] ++ cf, env)

fStatWhile :: (ValueOrAddress -> Env -> Code) -> CodeEnv -> CodeEnv
fStatWhile expr body = \env ->
    let
        (cb, _) = body env
        ce = expr Value env
        (n, k) = (codeSize cb, codeSize ce)
    in
        ([BRA n] ++ cb ++ ce ++ [BRT (-(n + k + 2))], env)

fStatReturn :: (ValueOrAddress -> Env -> Code) -> CodeEnv
fStatReturn expr = \env -> (expr Value env ++ [pop] ++ [RET], env)

fStatBlock :: [CodeEnv] -> CodeEnv
fStatBlock stats = \env ->
    foldl combine ([], env) stats
    where
        combine (c, e) stat =
            let (c', e') = stat e
            in (c ++ c', e')

fExprCon :: Token -> ValueOrAddress -> Env -> Code
fExprCon (ConstInt n) va env = [LDC n]
fExprCon (ConstBool False) va env = [LDC 0]
fExprCon (ConstBool True) va env = [LDC 1]
fExprCon (ConstChar c) va env = [LDC (ord c)]


fExprVar :: Token -> ValueOrAddress -> Env -> Code
fExprVar (LowerId x) va env =
    let
        loc = env ! x
    in
        case va of
            Value    ->  [LDL  loc]
            Address  ->  [LDLA loc]

fExprOp :: Token
    -> (ValueOrAddress -> Env -> Code)
    -> (ValueOrAddress -> Env -> Code)
    -> ValueOrAddress -> Env -> Code
fExprOp (Operator "=") lhs rhs va env = rhs Value env ++ [LDS 0] ++ lhs Address env ++ [STA 0]
fExprOp (Operator op)  lhs rhs va env = lhs Value env ++ rhs Value env ++ [opCodes ! op]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

fExprCall :: Token -> [ValueOrAddress -> Env -> Code] -> ValueOrAddress -> Env -> Code
fExprCall (LowerId ident) args va env = (args >>= (\f -> f va env)) ++ [Bsr ident]
