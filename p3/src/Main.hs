-- | Daan Rijks & Niek Mulleners
{-

  This is our final assignment for Talen en Compilers.
  We did not use comments everywhere, but tried to make our code easy to read
  and add comments only if really necessary.
  We did all the main tasks plus exercises 7, 8, 9 & 10.
  7:
  We added single- and multiline comments.
  8:
  We added both increment and decrement and assignment operations for all operators.
  9:
  We added a for statement like the one in C#, including the possibility to leave
  places empty.
  10:
  ppAlgebra provides a way to pretty print the AST, but make sure to use unlines as
  ppAlgebra returns a list of lines.

-}


module Main where

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived hiding ((<*))

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode
import CSharpPrettyPrint

start :: Parser s a -> [s] -> a
start p = fst . head . filter (null . snd) . parse p

main :: IO ()
main = do
         -- get command line arguments
         args  <- getArgs
         -- compute a list of input an output files
         files <- case args of
                    []  ->  do
                              putStrLn "no argument given; assuming example.cs"
                              return [("example.cs", "example.ssm")]
                    xs  ->  return (map (\ f -> (f, addExtension (dropExtension f) "ssm")) xs)
         -- translate each of the files
         mapM_ processFile files

-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) = do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
    where
        process = formatCode . codeGen . parseCls
        parseCls = start (pClass <* eof) . start lexicalScanner
        codeGen cls =
            let desugared = foldCSharp desugarAlgebra cls
            in foldCSharp codeAlgebra desugared (funcDecls cls)
