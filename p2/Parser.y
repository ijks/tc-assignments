{
module Parser where

import Scanner
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    "->"       { Arrow }
    "."        { RuleEnd }
    ","        { CmdSep }
    ";"        { AltSep }
    GO         { Keyword "go" }
    TAKE       { Keyword "take" }
    MARK       { Keyword "mark" }
    NOTHING    { Keyword "nothing" }
    TURN       { Keyword "turn" }
    CASE       { Keyword "case" }
    OF         { Keyword "of" }
    END        { Keyword "end" }
    LEFT       { Direction "left" }
    RIGHT      { Direction "right" }
    FRONT      { Direction "front" }
    "Empty"    { Pattern "Empty" }
    "Lambda"   { Pattern "Lambda" }
    "Debris"   { Pattern "Debris" }
    "Asteroid" { Pattern "Asteroid" }
    "Boundary" { Pattern "Boundary" }
    "_"        { Pattern "_" }
    IDENT      { Ident $$ }

%%

program
    : {- empty -} { [] }
    | program rule { $2 : $1 }

rule
    : IDENT "->" cmds "." { Rule $1 $3 }

cmds
    : {- empty -} { [] }
    | cmd { [$1] }
    | cmds "," cmd { $3 : $1 }

cmd
    : GO { Go }
    | TAKE { Take }
    | MARK { Mark }
    | NOTHING { NoOp }
    | TURN dir { Turn $2 }
    | CASE dir OF alts END { Case $2 $4 }
    | IDENT { CallRule $1 }

dir
    : LEFT { Parser.Left }
    | RIGHT { Parser.Right }
    | FRONT { Parser.Up }

alts
    : {- empty -} { [] }
    | alt { [$1] }
    | alts ";" alt { $3 : $1 }

alt
    : pat "->" cmds { ($1, $3) }

pat
    : "Empty" { Contents Empty }
    | "Lambda" { Contents Lambda }
    | "Debris" { Contents Debris }
    | "Asteroid" { Contents Asteroid }
    | "Boundary" { Contents Boundary }
    | "_" { Any }

{
-- We had to move the declaration of our abstact syntax here, because we would
-- have had a circular dependency between "Parser" and "Arrow" otherwise.

type Program = [Rule]

data Rule = Rule
    { ruleName :: Ident
    , ruleCommands :: Commands
    } deriving (Eq, Show)

type Commands = [Command]

data Command
    = Go | Take | Mark | NoOp
    | Turn Heading
    | Case Heading [Alternative]
    | CallRule Ident
    deriving (Eq, Show)

data Heading = Up | Down | Left | Right
    deriving (Eq, Show)

type Alternative = (Pattern, Commands)

type Ident = String

data Pattern
    = Any
    | Contents Contents
    deriving (Eq, Show)

data Contents = Empty | Lambda | Debris | Asteroid | Boundary
    deriving (Show, Enum, Eq)

parseError :: [Token] -> a
parseError _ = error "Parse error :("
}
