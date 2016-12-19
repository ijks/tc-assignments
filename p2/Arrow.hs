module Arrow where

import Prelude hiding ((<*), (<$), Left, Right)
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
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

type Env = Map Ident Commands

type Stack = Commands
data ArrowState = ArrowState Space Pos Heading Stack

data Step
    = Done Space Pos Heading
    | Ok ArrowState
    | Fail String

type ProgramAlgebra p r c h =
    ( [r] -> p -- program
    , Ident -> [c] -> r -- rule
    , c -- go
    , c -- take
    , c -- mark
    , c -- noOp
    , h -> c -- turn
    , h -> [(Pattern, [c])] -> c -- case
    , Ident -> c -- call
    , h -- left
    , h -- right
    , h -- front
    )

foldProgram :: ProgramAlgebra p r c h -> Program -> p
foldProgram
    ( p, r
    , go, tk, mark, noop, turn, cse, call
    , left, right, front) = fProgram
        where
            fProgram = p . map fRule
            fRule (Rule i cs) = r i (fCs cs)
            fCommand Go = go
            fCommand Take = tk
            fCommand Mark = mark
            fCommand NoOp = noop
            fCommand (Turn a) = turn (fHeading a)
            fCommand (Case a cs) = cse (fHeading a) (map (fmap fCs) cs)
            fCommand (Call i) = call i
            fHeading Left = left
            fHeading Right = right
            fHeading Front = front
            fCs = map fCommand

saneAlgebra :: ProgramAlgebra (Env -> Bool) (Env -> Env) (Env -> Env -> Bool) ()
saneAlgebra =
    -- Program
    ( \rules -> \env -> undefined --elem "start" rules && noDuplicates rules
    -- Rule
    , \i cs -> \env -> insert i cs env -- Ident -> [c] -> r
    -- Command
    , undefined -- c -- go
    , undefined -- c -- take
    , undefined -- c -- mark
    , undefined -- c -- noOp
    , undefined -- h -> c -- turn
    , \_ xs -> \env fenv -> correctPattern (lookup Any xs)
        || contains (map Contents [Empty, Lambda, Debris, Asteroid, Boundary]) xs -- case
    , undefined -- Ident -> c -- call
    -- Heading
    , (), (), ()
    )
    where
        correctPattern (Just cs) = and cs
        correctPattern Nothing = False
        contains ps cs = all correctPattern $ map (\x -> lookup x cs) ps
        noDuplicates [] = True
        noDuplicates (x:xs) = notElem x xs && noDuplicates xs 

{-
type ProgramAlgebra p r cs c h =
    (   ( r -> p -> p -- consRules
        , p -- emptyRules
        )
    ,   RuleAlgebra r cs c h
    )

type RuleAlgebra r cs c h =
    (   ( Ident -> cs -> r -- rule
        )
    ,   CommandsAlgebra cs c h
    )

type CommandsAlgebra cs c h =
    (   ( c -> cs -> cs -- consCommands
        , cs -- emptyCommands
        )
    ,   CommandAlgebra c h
    )

type CommandAlgebra c h =
    (   ( c -- go
        , c -- take
        , c -- mark
        , c -- noOp
        , h -> c -- turn
        , h -> [(Pattern, Commands)] -> c -- case
        , Ident -> c -- call
        )
    ,   HeadingAlgebra h
    )

type HeadingAlgebra h =
    ( h -- left
    , h -- right
    , h -- front
    )

foldProgram :: ProgramAlgebra p r cs c h -> Program -> p
foldProgram ((cons, empty), ruleAlgebra) = f
    where
        f (r:rs) = cons (foldRule ruleAlgebra r) (f rs)
        f [] = empty

foldRule :: RuleAlgebra r cs c h -> Rule -> r
foldRule (rule, commandsAlgebra) = f
    where
        f (Rule s cs) = rule s (foldCommands commandsAlgebra cs)

foldCommands :: CommandsAlgebra cs c h -> Commands -> cs
foldCommands ((cons, empty), commandAlgebra) = f
    where
        f (c:cs) = cons (foldCommand commandAlgebra c) (f cs)
        f [] = empty

foldCommand :: CommandAlgebra c h -> Command -> c
foldCommand ((go, tk, mark, noop, turn, cse, call), headingAlgebra) = f
    where
        f Go = go
        f Take = tk
        f Mark = mark
        f NoOp = noop
        f (Turn h) = turn (foldHeading headingAlgebra h)
        f (Case h cases) = cse (foldHeading headingAlgebra h) cases
        f (Call i) = call i

foldHeading :: HeadingAlgebra h -> Heading -> h
foldHeading (left, right, front) = f
    where
        f Left = left
        f Right = right
        f Front = front

saneAlgebra :: ProgramAlgebra (Env -> Bool) (Env -> Env) Bool Bool ()
saneAlgebra = prgrm <: rule <: cmds <: cmd <: hdng
    where
        infixr 5 <:
        (<:) = (,)
        prgrm = ((:), [])
        rule = const
        cmds = (\c cs -> c && cs, True)
        cmd = 
            (True, True, True, True, const True
            , \h cs -> length cs > 4 || case lookup Any cs of
                Just _ -> True
                Nothing -> False
            , const True)
        hdng = ((),(),())

check :: Program -> Bool
check = (\rules -> elem "start" rules && noDuplicates rules) . foldProgram saneAlgebra
    where
        noDuplicates [] = True
        noDuplicates (x:xs) = notElem x xs && noDuplicates xs 

-}