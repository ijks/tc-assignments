{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Arrow where

import Prelude hiding ((<*), (<$), Left, Right)

import Control.Arrow (second)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Char (isSpace)

import ParseLib.Abstract

type Size = Int
type Pos = (Int, Int)
type Space = Map Pos Contents
data Contents = Empty | Lambda | Debris | Asteroid | Boundary
    deriving (Show, Enum, Eq)

parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised
        ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ Map.fromList $ concat $
        zipWith (\r cs ->
            zipWith (\c d -> ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
    choice (map (\(c, f) -> f <$ symbol c) table) <* spaces
    where
        (=:) = (,)
        table =
            [ '.' =: Empty
            , '\\' =: Lambda
            , '%' =: Debris
            , 'O' =: Asteroid
            , '#' =: Boundary
            ]

data Pattern
    = Any
    | Contents Contents
    deriving (Eq, Show)

data Command
    = Go | Take | Mark | NoOp
    | Turn Heading
    | Case Heading [(Pattern, Commands)]
    | CallRule Ident
    deriving (Eq, Show)

type Commands = [Command]
type Ident = String
data Heading = Left | Right | Front
    deriving (Eq, Show)

data Rule = Rule
    { ruleName :: Ident
    , ruleCommands :: Commands
    } deriving (Eq, Show)

type Program = [Rule]

data ListAlgebra a r = ListA
    { cons :: a -> r -> r
    , nil :: r
    }

foldList :: ListAlgebra a r -> [a] -> r
foldList (ListA cons nil) = foldr cons nil

type ProgramAlgebra = ListAlgebra Rule

data CommandAlgebra r = CommandA
    { cmdGo :: r
    , cmdTake :: r
    , cmdMark :: r
    , cmdNoOp :: r
    , cmdTurn :: Heading -> r
    , cmdCase :: Heading -> [(Pattern, [r])] -> r
    , cmdCallRule :: Ident -> r
    }

foldCommand :: CommandAlgebra r -> Command -> r
foldCommand CommandA { .. } = fold
    where
        fold Go = cmdGo
        fold Take = cmdTake
        fold Mark = cmdMark
        fold NoOp = cmdNoOp
        fold (Turn h) = cmdTurn h
        fold (Case h ps) = cmdCase h (fmap (second (fmap fold)) ps)
        fold (CallRule rl) = cmdCallRule rl

type Env = Map Ident Commands

type Stack = Commands
data ArrowState = ArrowState Space Pos Heading Stack

data Step
    = Done Space Pos Heading
    | Ok ArrowState
    | Fail String

noUndefinedCallsA :: CommandAlgebra (Env -> Bool)
noUndefinedCallsA = CommandA t t t t
    (\_ _ -> True)
    (\_ ps env -> all (all ($ env) . snd) ps)
    (\rule env -> rule `Map.member` env)
    where t = const True

noRefutableCaseA :: CommandAlgebra Bool
noRefutableCaseA = CommandA True True True True
    (const True)
    (\_ ps -> checkPatterns (fst <$> ps) && and (and . snd <$> ps))
    (const True)
    where
        checkPatterns ps = any (== Any) ps || all (`elem` ps) allPossible
        allPossible = Contents <$> [Empty .. Boundary]

environmentA :: ProgramAlgebra (Maybe Env)
environmentA = ListA
    { cons = \(Rule name cmds) ->
        (>>= \env ->
            if name `Map.member` env
                then Nothing
                else Just (Map.insert name cmds env)
        )
    , nil = Just Map.empty
    }

startExistsA :: ProgramAlgebra Bool
startExistsA = ListA
    { cons = (||) . (== "start") . ruleName
    , nil = False
    }

check :: Program -> Bool
check p = and
    [ foldList startExistsA p
    , or $ do -- 'or' works on 'Foldable' types, which 'Maybe' is.
        env <- foldList environmentA p
        return . all (\c -> foldCommand noUndefinedCallsA c env) $ cmds
    , all (foldCommand noRefutableCaseA) cmds
    ]
    where cmds = p >>= ruleCommands
