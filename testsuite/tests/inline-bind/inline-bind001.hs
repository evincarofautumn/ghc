module Main where

import Data.IORef

main :: IO ()
main = do
  test1
  test2
  test3
  test4
  test5
  test6
  test7
  test8
  test9

-- A single binding.
test1 :: IO ()
test1 = do
  putStrLn (<- getLine)

-- A binding using ($).
test2 :: IO ()
test2 = do
  putStrLn $ <- getLine

-- Multiple bindings in the same statement.
test3 :: IO ()
test3 = do
  print $ replicate (<- readLn) (<- getLine)

-- Binding in 'let' statement.
test4 :: IO ()
test4 = do
  let x = <- getLine
  putStrLn x

-- Multiple inline bindings in a single 'let' binding.
test5 :: IO ()
test5 = do
  let x = (<- readLn) + (<- readLn) :: Int
  print x

-- Multiple inline bindings across multiple 'let' bindings.
test6 :: IO ()
test6 = do
  let x = <- readLn
      y = <- readLn
  print (x + y :: Int)

-- Inline bindings in a tuple.
test7 :: IO ()
test7 = do
  print (<- readLn :: Int, <- readLn :: Int)

-- Binding in 'if' condition.
test8 :: IO ()
test8 = do
  putStrLn $ if <- readLn then "test8-true" else "test8-false"

-- Binding within 'if' branches.
test9 :: IO ()
test9 = do
  x <- newIORef (0 :: Int)
  results <- while ((< 10) <$> readIORef x) $ do
    modifyIORef' x succ
    readIORef x
  print results
  where
    while :: (Monad m) => m Bool -> m a -> m [a]
    while condition body = loop []
      where
        loop acc = do
          if <- condition
            then do
              loop $ (<- body) : acc
            else pure $ reverse acc
