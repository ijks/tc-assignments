{
module Parser where

import Arrow
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
    | IDENT { Call $1 }

dir
    : LEFT { Arrow.Left }
    | RIGHT { Arrow.Right }
    | FRONT { Arrow.Front }

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
parseError :: [Token] -> a
parseError _ = error "Parse error :("
}
