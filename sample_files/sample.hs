{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Comprehensive Haskell sample demonstrating syntax highlighting
-- This module showcases all major Haskell language features
module Sample
    ( -- * Data Types
      Person(..)
    , Status(..)
    , Tree(..)
    , Expression(..)
      -- * Type Classes
    , Printable(..)
    , Container(..)
      -- * Functions
    , factorial
    , fibonacci
    , quicksort
    , main
    ) where

import Control.Monad (forM_, when, unless, guard, void)
import Control.Monad.State (State, StateT, get, put, modify, runState, evalStateT)
import Control.Monad.Reader (Reader, ReaderT, ask, asks, local, runReader)
import Control.Monad.Writer (Writer, WriterT, tell, execWriter)
import Control.Monad.Except (ExceptT, throwError, catchError, runExceptT)
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative (Alternative(..), (<|>), optional)
import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception (Exception, SomeException, try, catch, throw, bracket)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, isJust, isNothing)
import Data.Either (either, lefts, rights, partitionEithers)
import Data.List (sort, sortBy, group, groupBy, nub, partition, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Foldable (traverse_, for_, fold, foldMap)
import Data.Traversable (for, traverse)
import Data.Function ((&), on)
import Data.Functor ((<&>), ($>))
import Data.Kind (Type, Constraint)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat, Symbol, KnownNat, KnownSymbol, natVal, symbolVal)
import System.IO (Handle, IOMode(..), withFile, hPutStrLn, hGetLine)
import System.Environment (getArgs, getEnv, lookupEnv)
import Text.Printf (printf)

-- ============================================================================
-- Numeric Literals and Basic Types
-- ============================================================================

-- Integer literals
integerLiteral :: Integer
integerLiteral = 42

hexLiteral :: Int
hexLiteral = 0xFF

octalLiteral :: Int
octalLiteral = 0o755

binaryLiteral :: Int
binaryLiteral = 0b101010

-- Floating point literals
floatLiteral :: Float
floatLiteral = 3.14159

doubleLiteral :: Double
doubleLiteral = 2.71828e10

scientificNotation :: Double
scientificNotation = 6.022e23

-- Character and string literals
charLiteral :: Char
charLiteral = 'λ'

escapedChar :: Char
escapedChar = '\n'

stringLiteral :: String
stringLiteral = "Hello, Haskell!"

multilineString :: String
multilineString = "This is a \
                  \multiline string \
                  \in Haskell"

rawString :: String
rawString = "Escape sequences: \t \n \r \\ \""

-- ============================================================================
-- Algebraic Data Types
-- ============================================================================

-- Simple sum type (enumeration)
data Status
    = Active
    | Inactive
    | Pending
    | Deleted
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- Record type
data Person = Person
    { personId   :: !Int           -- Strict field
    , personName :: Text
    , personAge  :: Maybe Int
    , personTags :: [String]
    } deriving (Show, Eq, Generic)

-- Parameterized type (generic)
data Tree a
    = Leaf a
    | Branch (Tree a) (Tree a)
    deriving (Show, Eq, Functor)

-- Recursive data type with multiple constructors
data Expression
    = Literal Int
    | Variable String
    | Add Expression Expression
    | Multiply Expression Expression
    | Lambda String Expression
    | Apply Expression Expression
    deriving (Show, Eq)

-- Newtype for type safety
newtype UserId = UserId { unUserId :: Int }
    deriving (Show, Eq, Ord, Num)

newtype Email = Email Text
    deriving (Show, Eq)

-- GADT (Generalized Algebraic Data Type)
data Expr a where
    EInt    :: Int -> Expr Int
    EBool   :: Bool -> Expr Bool
    EAdd    :: Expr Int -> Expr Int -> Expr Int
    EEq     :: Eq a => Expr a -> Expr a -> Expr Bool
    EIf     :: Expr Bool -> Expr a -> Expr a -> Expr a

-- Type family
type family Element (container :: Type) :: Type where
    Element [a]       = a
    Element (Maybe a) = a
    Element (Set a)   = a

-- Associated type family in type class
class Container c where
    type Elem c :: Type
    empty :: c
    insert :: Elem c -> c -> c
    member :: Elem c -> c -> Bool

instance Ord a => Container (Set a) where
    type Elem (Set a) = a
    empty = Set.empty
    insert = Set.insert
    member = Set.member

-- ============================================================================
-- Type Classes
-- ============================================================================

-- Simple type class
class Printable a where
    printValue :: a -> String

    -- Default implementation
    printList :: [a] -> String
    printList xs = "[" ++ intercalate ", " (map printValue xs) ++ "]"

instance Printable Int where
    printValue = show

instance Printable Bool where
    printValue True  = "yes"
    printValue False = "no"

instance Printable a => Printable (Maybe a) where
    printValue Nothing  = "nothing"
    printValue (Just x) = "just " ++ printValue x

-- Multi-parameter type class with functional dependency
class Convertible a b | a -> b where
    convert :: a -> b

instance Convertible Int Integer where
    convert = toInteger

instance Convertible String Text where
    convert = T.pack

-- ============================================================================
-- Functions and Pattern Matching
-- ============================================================================

-- Simple recursive function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- With guards
factorial' :: Integer -> Integer
factorial' n
    | n < 0     = error "Negative input"
    | n == 0    = 1
    | otherwise = n * factorial' (n - 1)

-- Fibonacci with pattern matching
fibonacci :: Int -> Integer
fibonacci = (fibs !!)
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- QuickSort implementation
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

-- Function with multiple clauses and guards
classify :: (Ord a, Num a) => a -> String
classify x
    | x < 0     = "negative"
    | x == 0    = "zero"
    | x < 10    = "small"
    | x < 100   = "medium"
    | otherwise = "large"

-- As-patterns and nested patterns
firstTwo :: [a] -> Maybe (a, a)
firstTwo xs@(a:b:_) = Just (a, b)
firstTwo _          = Nothing

-- View patterns and pattern guards
describeList :: [a] -> String
describeList xs
    | null xs        = "empty"
    | [_] <- xs      = "singleton"
    | [_, _] <- xs   = "pair"
    | length xs < 10 = "short"
    | otherwise      = "long"

-- ============================================================================
-- Higher-Order Functions
-- ============================================================================

-- Function taking function as argument
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Function returning function (currying)
add :: Int -> Int -> Int
add x y = x + y

addFive :: Int -> Int
addFive = add 5

-- Composition
processData :: String -> String
processData = map toUpper . filter isAlpha . trim
  where
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Fold examples
sumList :: Num a => [a] -> a
sumList = foldl' (+) 0
  where
    foldl' f z []     = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

productList :: Num a => [a] -> a
productList = foldr (*) 1

-- Point-free style
average :: [Double] -> Double
average = uncurry (/) . foldr (\x (s, c) -> (s + x, c + 1)) (0, 0)

-- ============================================================================
-- Monads and Do-Notation
-- ============================================================================

-- Maybe monad
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeComputation :: Double -> Double -> Double -> Maybe Double
safeComputation a b c = do
    x <- safeDivide a b
    y <- safeDivide x c
    return (x + y)

-- Alternative with applicative
safeComputation' :: Double -> Double -> Double -> Maybe Double
safeComputation' a b c = (+) <$> safeDivide a b <*> safeDivide a c

-- List monad for non-determinism
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- With guards
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = do
    a <- [1..n]
    b <- [a..n]
    c <- [b..n]
    guard (a*a + b*b == c*c)
    return (a, b, c)

-- State monad
type Counter = State Int

increment :: Counter ()
increment = modify (+1)

decrement :: Counter ()
decrement = modify (subtract 1)

getCount :: Counter Int
getCount = get

runCounter :: Counter a -> Int -> (a, Int)
runCounter = runState

-- Reader monad for configuration
data Config = Config
    { configHost :: String
    , configPort :: Int
    , configDebug :: Bool
    }

type App = Reader Config

getFullAddress :: App String
getFullAddress = do
    host <- asks configHost
    port <- asks configPort
    return $ host ++ ":" ++ show port

-- Writer monad for logging
type Logger = Writer [String]

logMessage :: String -> Logger ()
logMessage msg = tell [msg]

computeWithLogging :: Int -> Logger Int
computeWithLogging n = do
    logMessage $ "Starting computation with " ++ show n
    let result = n * 2
    logMessage $ "Result is " ++ show result
    return result

-- Monad transformer stack
type AppM = ReaderT Config (StateT Int (ExceptT String IO))

runAppM :: Config -> Int -> AppM a -> IO (Either String (a, Int))
runAppM cfg st action = runExceptT $ runStateT (runReaderT action cfg) st

-- ============================================================================
-- IO and Effects
-- ============================================================================

-- Basic IO
greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ "!"

-- File handling
readFileContents :: FilePath -> IO (Either String Text)
readFileContents path = do
    result <- try (TIO.readFile path)
    return $ case result of
        Left (e :: SomeException) -> Left (show e)
        Right contents -> Right contents

writeToFile :: FilePath -> Text -> IO ()
writeToFile path content = TIO.writeFile path content

-- Exception handling
safeOperation :: IO a -> IO (Maybe a)
safeOperation action = do
    result <- try action
    return $ case result of
        Left (_ :: SomeException) -> Nothing
        Right value -> Just value

withResource :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
withResource acquire release = bracket acquire release

-- IORef for mutable state
counterExample :: IO Int
counterExample = do
    ref <- newIORef (0 :: Int)
    replicateM_ 10 $ modifyIORef ref (+1)
    readIORef ref
  where
    replicateM_ n action = sequence_ (replicate n action)

-- ============================================================================
-- Concurrency
-- ============================================================================

-- Basic threading
concurrentExample :: IO ()
concurrentExample = do
    mvar <- newMVar (0 :: Int)

    -- Spawn worker threads
    _ <- forkIO $ worker mvar "A" 3
    _ <- forkIO $ worker mvar "B" 3

    -- Wait a bit
    threadDelay 1000000

    -- Read final value
    final <- takeMVar mvar
    putStrLn $ "Final value: " ++ show final
  where
    worker mvar name n = forM_ [1..n] $ \i -> do
        val <- takeMVar mvar
        putStrLn $ name ++ ": " ++ show val
        putMVar mvar (val + 1)
        threadDelay 100000

-- STM (Software Transactional Memory)
stmExample :: IO ()
stmExample = do
    account1 <- atomically $ newTVar (1000 :: Int)
    account2 <- atomically $ newTVar (500 :: Int)

    -- Transfer money
    atomically $ transfer account1 account2 200

    -- Check balances
    (b1, b2) <- atomically $ (,) <$> readTVar account1 <*> readTVar account2
    printf "Balances: %d, %d\n" b1 b2
  where
    transfer :: TVar Int -> TVar Int -> Int -> STM ()
    transfer from to amount = do
        fromBalance <- readTVar from
        when (fromBalance >= amount) $ do
            writeTVar from (fromBalance - amount)
            toBalance <- readTVar to
            writeTVar to (toBalance + amount)

-- ============================================================================
-- Advanced Features
-- ============================================================================

-- Lazy evaluation and infinite lists
naturals :: [Integer]
naturals = [0..]

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

-- Bang patterns for strictness
strictSum :: Num a => [a] -> a
strictSum = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs

-- Corecursion
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- Existential types (via GADT)
data Showable where
    MkShowable :: Show a => a -> Showable

instance Show Showable where
    show (MkShowable x) = show x

heterogeneousList :: [Showable]
heterogeneousList = [MkShowable (42 :: Int), MkShowable "hello", MkShowable True]

-- Rank-N types
runST' :: (forall s. ST s a) -> a
runST' = undefined  -- Implementation would use actual ST

-- Type-level programming
data Nat' = Zero | Succ Nat'

type family Add (m :: Nat') (n :: Nat') :: Nat' where
    Add 'Zero n     = n
    Add ('Succ m) n = 'Succ (Add m n)

-- Phantom types
data Validated
data Unvalidated

newtype Form (status :: Type) = Form { formData :: Map String String }

validateForm :: Form Unvalidated -> Either String (Form Validated)
validateForm (Form m)
    | Map.member "email" m && Map.member "name" m = Right (Form m)
    | otherwise = Left "Missing required fields"

submitForm :: Form Validated -> IO ()
submitForm (Form m) = putStrLn $ "Submitting: " ++ show m

-- ============================================================================
-- Evaluation with GADT
-- ============================================================================

evalExpr :: Expr a -> a
evalExpr (EInt n)      = n
evalExpr (EBool b)     = b
evalExpr (EAdd e1 e2)  = evalExpr e1 + evalExpr e2
evalExpr (EEq e1 e2)   = evalExpr e1 == evalExpr e2
evalExpr (EIf c t e)   = if evalExpr c then evalExpr t else evalExpr e

-- Example expressions
exampleExpr :: Expr Int
exampleExpr = EAdd (EInt 10) (EIf (EBool True) (EInt 5) (EInt 0))

-- ============================================================================
-- Main Entry Point
-- ============================================================================

main :: IO ()
main = do
    putStrLn "=== Haskell Sample Program ==="

    -- Basic functions
    putStrLn $ "Factorial 10: " ++ show (factorial 10)
    putStrLn $ "Fibonacci 20: " ++ show (fibonacci 20)
    putStrLn $ "Quicksort: " ++ show (quicksort [3, 1, 4, 1, 5, 9, 2, 6])

    -- Algebraic data types
    let person = Person 1 "Alice" (Just 30) ["developer", "haskeller"]
    print person

    -- Tree operations
    let tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
    print $ fmap (*2) tree

    -- Monad examples
    print $ safeComputation 10 2 5
    print $ pythagoreanTriples 20

    -- GADT evaluation
    putStrLn $ "Expression result: " ++ show (evalExpr exampleExpr)

    -- Heterogeneous list
    mapM_ print heterogeneousList

    putStrLn "Done!"
