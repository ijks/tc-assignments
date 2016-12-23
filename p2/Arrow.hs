{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Arrow where

-- ** Written assignments are included at the bottom of the file ** --

import Prelude hiding ((<*), (<$), Left, Right)
import Control.Arrow (second)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (find)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)

import ParseLib.Abstract

import Parser
import Scanner

type Size = Int
type Pos = (Int, Int)
type Space = Map Pos Contents

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
    choice (map (\(f, c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents, Char)]
contentsTable =
    [ (Empty , '.')
    , (Lambda, '\\')
    , (Debris, '%')
    , (Asteroid, 'O')
    , (Boundary, '#')
    ]

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

-- Check whether there are no calls to undefined rules.
noUndefinedCallsA :: CommandAlgebra (Env -> Bool)
noUndefinedCallsA = CommandA t t t t
    (\_ _ -> True)
    (\_ ps env -> all (all ($ env) . snd) ps)
    (\rule env -> rule `Map.member` env)
    where t = const True

-- Check whether there are no case statements which could fail.
noRefutableCaseA :: CommandAlgebra Bool
noRefutableCaseA = CommandA True True True True
    (const True)
    (\_ ps -> checkPatterns (fst <$> ps) && and (and . snd <$> ps))
    (const True)
    where
        checkPatterns ps = any (== Any) ps || all (`elem` ps) allPossible
        allPossible = Contents <$> [Empty .. Boundary]

-- Produce the environment for a program, but only if it has no duplicate rules.
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

-- Check whether a rule named "start" exists
startExistsA :: ProgramAlgebra Bool
startExistsA = ListA
    { cons = (||) . (== "start") . ruleName
    , nil = False
    }

-- Check whether a program is valid.
check :: Program -> Bool
check = isJust . check'

-- Check whether a program is valid, and if so, return its environment.
-- This way, we don't have to calculate the environment twice in 'toEnvironment'.
check' :: Program -> Maybe Env
check' p = do
    let cmds = p >>= ruleCommands
    let startExists = foldList startExistsA p
    let cmdsValid = all (foldCommand noRefutableCaseA) cmds
    if startExists && cmdsValid
        then do
            env <- foldList environmentA p
            if all (\c -> foldCommand noUndefinedCallsA c env) cmds
                then Just env
                else Nothing
        else
            Nothing

-- Produce a graphical representation of a space.
printSpace :: Space -> String
printSpace space =
    show size ++ "\n" ++
    foldr f "" coords
    where
        noDuplicates [] = True
        noDuplicates (x:xs) = notElem x xs && noDuplicates xs
        -- We know that the last coordinate is always the size of the space
        -- as long as the space is correct, according to the documentation of Map.
        size = last coords
        f pos rest = print pos ++ (if endOfLine pos then "\n" else "") ++ rest
            where
                print = (:[]) . fromJust . (flip lookup $ contentsTable) . (space !)
                endOfLine = (== snd size) . snd
        coords = Map.keys space

type Env = Map Ident Commands

-- Parse a string into a program, check it, and return the program's environment.
-- Note: not total, can call 'error'. We would have used 'Maybe' or 'Either'
-- here, but since the type of this function is given in the assignment, we felt
-- we shouldn't change it.
toEnvironment :: String -> Env
toEnvironment s =
    case check' . Parser.parse . Scanner.scan $ s of
        Just env -> env
        Nothing -> error "invalid program"

type Stack = Commands

data ArrowState = ArrowState Space Pos Heading Stack

data Step
    = Done Space Pos Heading
    | Ok ArrowState
    | Fail String

step :: Env -> ArrowState -> Step
step _ (ArrowState space pos heading []) =
    Done space pos heading
step env (ArrowState space (pos @ (x, y)) heading (cmd:stack)) =
    case cmd of
        Go ->
            Ok $ case Map.lookup forward space of
                Just Empty -> ArrowState space forward heading stack
                Just Lambda -> ArrowState space forward heading stack
                Just Debris -> ArrowState space forward heading stack
                _ -> ArrowState space pos heading stack
        Take ->
            Ok $ ArrowState (Map.adjust takeContents pos space) pos heading stack
        Mark ->
            Ok $ ArrowState (Map.insert pos Lambda space) pos heading stack
        NoOp ->
            Ok $ ArrowState space pos heading stack
        Turn dir ->
            Ok $ ArrowState space pos (turn dir heading) stack
        Case h alts ->
            case find (matches forwardContents . fst) alts of
                Just (_, cmds) ->
                    Ok $ ArrowState space pos heading (cmds ++ stack)
                Nothing ->
                    Fail "no matching pattern found!"
        CallRule rule ->
            case Map.lookup rule env of
                Just cmds ->
                    Ok $ ArrowState space pos heading (cmds ++ stack)
                Nothing ->
                    Fail $ "rule \"" ++ rule ++ "\" not found!"
    where
        -- Our next position, provided there is nothing in the way.
        forward = case heading of
            Up -> (x, y + 1)
            Down -> (x, y - 1)
            Left -> (x - 1, y)
            Right -> (x + 1, y)
        -- The contents of the space in front of us.
        forwardContents = fromMaybe Boundary (Map.lookup forward space)

takeContents :: Contents -> Contents
takeContents Lambda = Empty
takeContents Debris = Empty
takeContents c = c

-- Turn a heading left or right. Up and down are ignored.
turn :: Heading -> Heading -> Heading
turn Left h = case h of
    Up -> Left
    Left -> Down
    Down -> Right
    Right -> Up
turn Right h = case h of
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up
turn _ h = h

-- Check if contents match a pattern.
matches :: Contents -> Pattern -> Bool
matches cts (Contents c) = c == cts
matches _ Any = True

test = do
    s <- readFile "examples\\AddInput.space"
    let sp = fst . head $ ParseLib.Abstract.parse parseSpace s
    t <- readFile "examples\\Add.arrow"
    let ts = Scanner.scan t
    let ps = Parser.parse ts
    interactive (fromJust $ check' ps) (ArrowState sp (0,0) Right [CallRule "start"])


interactive :: Env -> ArrowState -> IO ()
interactive env state @ (ArrowState space _ _ _) = do
    putStrLn $ printSpace space
    let stp = step env state
    actUpon stp
    where
        actUpon (Done space pos heading) = putStrLn "done"
        actUpon (Ok st) = interactive env st
        actUpon (Fail s) = putStrLn s

-- * Exercise 4
-- The documentation on Happy states that it is more efficient when the grammar
-- is left-recursive. https://www.haskell.org/happy/doc/html/sec-sequences.html
-- This is different than parser combinators, which prefer a right-recursive
-- grammar, and in fact fail in the case of a left-recursive grammar.

-- * Exercise 10
-- A recursive call adds all commands of the called rule to the top of the
-- stack, while leaving the other commands of the current rule below it. When
-- the recursive call is at the end of the command sequence, there are no
-- commands left on the stack of the initial command sequence. If the recursive
-- call is in the middle of the sequence, or even at the front, all commands
-- that come after it will remain on the stack.
-- Therefore: the earlier the recursive call is in a command sequence, the more
-- impact it has on the size of the stack.
