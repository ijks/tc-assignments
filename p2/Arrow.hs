module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (replicateM)
import Data.Char (isSpace)

type Size = Int
type Pos = (Int, Int)
type Space = Map Pos Contents
data Contents = Empty | Lambda | Debris | Asteroid | Boundary
    deriving (Show, Eq)

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

type Environment = Map Ident Commands

type Stack = Commands
data ArrowState = ArrowState Space Pos Heading Stack

data Step
    = Done Space Pos Heading
    | Ok ArrowState
    | Fail String
