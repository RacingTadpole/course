{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
-- call this with  :main "argument"
main ::
  IO ()
main =
  getArgs >>= \l ->
    case l of
      h :. _ -> run h
      Nil -> putStrLn "pass an argument ya dingbat"


type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars  -- the filename that contains the other file names
  -> IO ()
run path = 
  getFile path >>= \(_, q) ->
  getFiles (lines q) >>= \x ->
  printFiles x

-- run path =
--   do  q <- readFile path
--       x <- getFiles (lines q)  -- cannot use applicative notation because q on LHS & RHS
--       printFiles x

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
-- getFiles Nil = pure Nil
-- getFiles (h :. t) =
--   _undef
--   getFile h <*>
--   getFiles t

-- Note getFiles ps = getFile <$> ps  -- :: List( IO (FilePath, Chars))
-- we need sequence to turn List (IO ...) to IO (List ...)
getFiles = sequence . (<$>) getFile  -- sequence . (<$>) == traverse

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path =
  readFile path >>= \c ->
  pure (path, c)

-- or getFile path = (\q -> (path, q)) <$> readFile path
-- or getFile path = ((,) path) <$> readFile path
-- or getFile = \p -> (<$>) ((,) p) (readFile p)
-- or getFile = lift2 (<$>) (,) readFile

-- note two applicatives compose to make a new applicative
-- see :t lift2 (<*>)
  
-- Eg. printFiles (("mypath", "stuff") :. ("path2", "styf") :. Nil)
printFiles ::
  List (FilePath, Chars)
  -> IO ()
-- printFiles Nil = pure ()
-- printFiles ((x, y) :. t) =
--   printFile x y <*
--   putStrLn ("") <*
--   printFiles t <*
--   pure ()

-- printFiles ps =
--   void (sequence ((\(p, c) -> printFile p c) <$> ps))
--   void (sequence ((\(p, c) -> uncurry printFile (p, c)) <$> ps))
--   void (sequence ((<$>) (uncurry printFile) ps))
--   void . sequence . (<$>) (uncurry printFile) ps
printFiles = void . sequence . (<$>) (uncurry printFile)

-- printFiles (h :. t) =
--   printFile (curry) <*  -- not right yet, but on the right track
--   putStrLn ("") <*
--   printFiles t <*
--   pure ()

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path contents =
  -- putStrLn ("============ " ++ path) >>= \_ ->
  -- putStrLn (contents) >>= \_ ->
  -- pure ()
  --
  -- putStrLn ("============ " ++ path) <*
  -- putStrLn (contents) <*
  -- pure ()
  --
  putStrLn ("============ " ++ path ++ "\n" ++ contents)  -- Tony says is more readable

  -- do
  --   x <- putStrLn (s)
  --   y <- putStrLn(path)
  --   pure()
  

