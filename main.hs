module Main where

import Data.Char (chr, ord)
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtension)
import System.IO (hFlush, stdout)
import System.Process (callProcess)

data Token = MovRight | MovLeft | Inc | Dec | Put | Get | Loop [Token] deriving (Show, Eq)

data Operation = ORight Int | OLeft Int | Add Int | Sub Int | Write Int | Read Int | While [Operation] deriving (Show, Eq)

parse :: String -> [Token]
parse [] = []
parse (c : cs) =
  case c of
    '>' -> MovRight : parse cs
    '<' -> MovLeft : parse cs
    '+' -> Inc : parse cs
    '-' -> Dec : parse cs
    '.' -> Put : parse cs
    ',' -> Get : parse cs
    '[' -> let (loopContent, rest) = getLoop cs in Loop loopContent : parse rest
    _ -> parse cs

findClosing :: String -> Int -> Int -> Int
findClosing [] _ _ = error "no closing ']' found"
findClosing (c : cs) n stack =
  case c of
    '[' -> findClosing cs (n + 1) (stack + 1)
    ']' ->
      if stack == 0
        then n
        else findClosing cs (n + 1) (stack - 1)
    _ -> findClosing cs (n + 1) stack

getLoop :: String -> ([Token], String)
getLoop s =
  let (first, second) = splitAt (findClosing s 0 0) s
   in (parse first, drop 1 second)

data Tape = Tape [Int] Int [Int]

emptyTape :: Tape
emptyTape = Tape [] 0 []

getCell :: Tape -> Int
getCell (Tape _ curr _) = curr

setCell :: Int -> Tape -> Tape
setCell n (Tape left _ right) = Tape left n right

checkZero :: Tape -> Bool
checkZero (Tape _ curr _) = curr == 0

moveRight :: Tape -> Tape
moveRight (Tape left curr []) = Tape (curr : left) 0 []
moveRight (Tape left curr (r : rs)) = Tape (curr : left) r rs

moveLeft :: Tape -> Tape
moveLeft (Tape [] curr right) = Tape [] 0 (curr : right)
moveLeft (Tape (l : ls) curr right) = Tape ls l (curr : right)

inc :: Tape -> Tape
inc (Tape left curr right) =
  if curr == 255
    then Tape left 0 right
    else Tape left (curr + 1) right

dec :: Tape -> Tape
dec (Tape left curr right) =
  if curr == 0
    then Tape left 255 right
    else Tape left (curr - 1) right

tapePrint :: Tape -> String
tapePrint (Tape _ v _) = [chr v]

run :: Tape -> [Token] -> IO Tape
run t [] = pure t
run t (x : xs) =
  case x of
    MovRight -> run (moveRight t) xs
    MovLeft -> run (moveLeft t) xs
    Inc -> run (inc t) xs
    Dec -> run (dec t) xs
    Put -> do
      putChar (chr (getCell t))
      run t xs
    Get -> do
      c <- getChar
      run (setCell (ord c) t) xs
    Loop body ->
      if checkZero t
        then run t xs
        else do
          tAfterBody <- run t body
          run tAfterBody (Loop body : xs)

optimize :: [Token] -> [Operation]
optimize [] = []
optimize ts = normalize (fixpoint peephole (group ts))

group :: [Token] -> [Operation]
group [] = []
group (t : ts) =
  case t of
    MovRight -> let n = collect 1 t ts in ORight n : group (drop (n - 1) ts)
    MovLeft -> let n = collect 1 t ts in OLeft n : group (drop (n - 1) ts)
    Inc -> let n = collect 1 t ts in Add n : group (drop (n - 1) ts)
    Dec -> let n = collect 1 t ts in Sub n : group (drop (n - 1) ts)
    Put -> let n = collect 1 t ts in Write n : group (drop (n - 1) ts)
    Get -> let n = collect 1 t ts in Read n : group (drop (n - 1) ts)
    Loop body -> While (group body) : group ts

collect :: Int -> Token -> [Token] -> Int
collect n _ [] = n
collect n t (x : xs) =
  if t == x
    then collect (n + 1) x xs
    else n

peephole :: [Operation] -> [Operation]
peephole [] = []
peephole [x] = [x]
peephole (ORight a : ORight b : xs) = peephole (ORight (a + b) : xs)
peephole (OLeft a : OLeft b : xs) = peephole (OLeft (a + b) : xs)
peephole (Add a : Add b : xs) = peephole (Add (a + b) : xs)
peephole (Sub a : Sub b : xs) = peephole (Sub (a + b) : xs)
peephole (Write a : Write b : xs) = peephole (Write (a + b) : xs)
peephole (Read a : Read b : xs) = peephole (Read (a + b) : xs)
peephole (ORight a : OLeft b : xs)
  | a == b = peephole xs
  | a > b = peephole (ORight (a - b) : xs)
  | b > a = peephole (OLeft (b - a) : xs)
peephole (OLeft a : ORight b : xs)
  | a == b = peephole xs
  | a > b = peephole (OLeft (a - b) : xs)
  | b > a = peephole (ORight (b - a) : xs)
peephole (Add a : Sub b : xs)
  | a == b = peephole xs
  | a > b = peephole (Add (a - b) : xs)
  | b > a = peephole (Sub (b - a) : xs)
peephole (Sub a : Add b : xs)
  | a == b = peephole xs
  | a > b = peephole (Sub (a - b) : xs)
  | b > a = peephole (Add (b - a) : xs)
peephole (While body : xs) = While (peephole body) : peephole xs
peephole (x : xs) = x : peephole xs

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
   in if x' == x
        then x
        else fixpoint f x'

normByte :: Int -> Int
normByte n = n `mod` 256

normalize :: [Operation] -> [Operation]
normalize [] = []
normalize (Add n : xs) =
  case normByte n of
    0 -> normalize xs
    k -> Add k : normalize xs
normalize (Sub n : xs) =
  case normByte n of
    0 -> normalize xs
    k -> Sub k : normalize xs
normalize (ORight n : xs)
  | n == 0 = normalize xs
  | otherwise = ORight n : normalize xs
normalize (OLeft n : xs)
  | n == 0 = normalize xs
  | otherwise = OLeft n : normalize xs
normalize (Write n : xs)
  | n <= 0 = normalize xs
  | otherwise = Write n : normalize xs
normalize (Read n : xs)
  | n <= 0 = normalize xs
  | otherwise = Read n : normalize xs
normalize (While body : xs) =
  While (normalize body) : normalize xs

compile :: [Operation] -> String
compile [] = "format ELF64 executable 3\nentry start\n\nsegment readable writeable\ntape rb 30000\noutbuf rb 4096\n\nsegment readable executable\nstart:\n\tmov r12, tape\n\tmov rax, 60\n\txor rdi, rdi\n\tsyscall"
compile t =
  let (program, _) = lower t 0
   in "format ELF64 executable 3\nentry start\n\nsegment readable writeable\ntape rb 30000\noutbuf rb 4096\n\nsegment readable executable\nstart:\n\tmov r12, tape"
        ++ program
        ++ "\n\tmov rax, 60\n\txor rdi, rdi\n\tsyscall"

lower :: [Operation] -> Int -> (String, Int)
lower [] loopId = ("", loopId)
lower (t : ts) loopId =
  let (thisAsm, afterThisId) = bfAsmMap t loopId
      (restAsm, finalId) = lower ts afterThisId
   in (thisAsm ++ restAsm, finalId)

readOnce :: String
readOnce = "\n\tmov rax, 1\n\tmov rdi, 1\n\tmov rsi, r12\n\tmov rdx, 1\n\tsyscall"

bfAsmMap :: Operation -> Int -> (String, Int)
bfAsmMap token loopId =
  case token of
    ORight n -> ("\n\tadd r12, " ++ show n, loopId)
    OLeft n -> ("\n\tsub r12, " ++ show n, loopId)
    Add n -> ("\n\tadd byte [r12], " ++ show n, loopId)
    Sub n -> ("\n\tsub byte [r12], " ++ show n, loopId)
    Write n -> ("\n\tmov al, [r12]\n\tmov rdi, outbuf\n\tmov rcx, " ++ show n ++ "\n\trep stosb\n\tmov rax, 1\n\tmov rdi, 1\n\tmov rsi, outbuf\n\tmov rdx, " ++ show n ++ "\n\tsyscall", loopId)
    Read n -> (concat (replicate n readOnce), loopId)
    While body ->
      let currentId = loopId
          (bodyAsm, nextId) = lower body (loopId + 1)
       in ( "\n.loop_start_"
              ++ show currentId
              ++ ":"
              ++ "\n\tcmp byte [r12], 0\n\tje .loop_end_"
              ++ show currentId
              ++ bodyAsm
              ++ "\n\tcmp byte [r12], 0\n\tjne .loop_start_"
              ++ show currentId
              ++ "\n.loop_end_"
              ++ show currentId
              ++ ":",
            nextId
          )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStr usage
    ["-h"] -> putStr usage
    [inputFile] -> compileFile inputFile (dropExtension inputFile)
    [inputFile, "-o", outputFile] -> compileFile inputFile outputFile
    ["-o", outputFile, inputFile] -> compileFile inputFile outputFile
    ["run", inputFile] -> runFile inputFile
    ["test", inputFile] -> do
      source <- readFile inputFile
      let ops = compile (optimize (parse source))
      putStrLn ops
    _ -> do
      putStr usage
      exitFailure

compileFile :: FilePath -> FilePath -> IO ()
compileFile inputFile outputFile = do
  source <- readFile inputFile

  let asmFile = outputFile ++ ".asm"
      asm = compile (optimize (parse source))

  writeFile asmFile asm
  callProcess "fasm" [asmFile, outputFile]
  removeFile asmFile

runFile :: FilePath -> IO ()
runFile inputFile = do
  source <- readFile inputFile
  _ <- run emptyTape (parse source)
  pure ()

usage :: String
usage =
  unlines
    [ "Usage:",
      "  bf <file.bf>",
      "  bf <file.bf> -o <binary>",
      "  bf run <file.bf>                   # Uses the inbuilt interpreter"
    ]
