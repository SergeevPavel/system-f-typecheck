module Main where

import Prelude hiding (succ, id, not)

import Test.HUnit

import TypeCheck

varNotFound :: Test
varNotFound = (Left "Unknown variable: x") ~=? (runTypeCheck $ EVar "x")

-- In context Nat
tNat = TVar "Nat"
zero = EVar "zero"
succ = EVar "succ"
one  = succ :@: zero
two  = succ :@: one
ctxWithNat = fromListContext [ TAs "Nat"
                             , EAs "zero" tNat
                             , EAs "succ" (tNat :->: tNat)
                             ]
runTypeCheckWithNat = runTypeCheckInContext ctxWithNat

runTypeCheckTT :: Expr -> IO ()
runTypeCheckTT expr = case runTypeCheckWithNat expr of Left errMsg -> putStrLn errMsg
                                                       Right t     -> (putStrLn . show) t

functionType :: Test
functionType = (Right $ tNat :->: tNat) ~=? (runTypeCheckWithNat expr)
    where
        expr = (Abs ("x", tNat) (EVar "x"))


id = TAbs "Y" $ Abs ("y", TVar "Y") (EVar "y")


polyIdType :: Test
polyIdType = (Right $ ForAll "Y" (TVar "Y" :->: TVar "Y")) ~=? (runTypeCheck id)


simpleApplication :: Test
simpleApplication = (Right $ tNat) ~=? (runTypeCheckWithNat expr)
    where
        expr = (Abs ("x", tNat) (EVar "x")) :@: one


simpleTypeApplication :: Test
simpleTypeApplication = (Right $ tNat :->: tNat) ~=? (runTypeCheckWithNat expr)
    where
        expr = id :$: tNat


-- double example
double = TAbs "X" $ Abs ("f", tX :->: tX) (Abs ("y", tX) (f :@: (f :@: y)))
    where
        tX = TVar "X"
        y  = EVar "y"
        f  = EVar "f"

doubleType = ForAll "X" ((tX :->: tX) :->: tX :->: tX)
    where
        tX = TVar "X"

quadruple = TAbs "X" $ double :$: (tX :->: tX) :@: (double :$: tX)
    where
        tX = TVar "X"


doubleTypeCheck :: Test
doubleTypeCheck = (Right $ doubleType) ~=? (runTypeCheckWithNat double)
    where
        tX = TVar "X"


quadrupleTypeCheck :: Test
quadrupleTypeCheck = (Right $ doubleType) ~=? (runTypeCheckWithNat quadruple)


doubleNat :: Test
doubleNat = (Right tNat) ~=? (runTypeCheckWithNat expr)
    where
        expr = double :$: tNat :@: succ :@: two


selfApp = Abs ("x", ForAll "X" (tX :->: tX)) (x :$: (ForAll "X" (tX :->: tX)) :@: x)
    where
        x  = EVar "x"
        tX = TVar "X"


selfAppType = (ForAll "X" (tX :->: tX)) :->: (ForAll "X" (tX :->: tX))
    where
        tX = TVar "X"


--doubleSelfApp :: Test
--doubleSelfApp = (Right selfAppType) ~=? (runTypeCheckWithNat $ selfApp :@: selfApp)


selfAppTypeCheck :: Test
selfAppTypeCheck = (Right selfAppType) ~=? (runTypeCheckWithNat selfApp)


tru = TAbs "Y" (Abs ("t", tY) (Abs ("f", tY) (t)))
    where
        tY = TVar "Y"
        t  = EVar "t"


fls = TAbs "Y" (Abs ("t", tY) (Abs ("f", tY) (f)))
    where
        tY = TVar "Y"
        f  = EVar "f"


tBool :: Type
tBool = ForAll "X" $ tX :->: tX :->: tX
    where
        tX = TVar "X"


not = Abs ("b", tBool) (TAbs "X" $ Abs ("t", tX) (Abs ("f", tX) $ b :$: tX :@: f :@: t))
    where
        tX = TVar "X"
        b  = EVar "b"
        f  = EVar "f"
        t  = EVar "t"

-- task 2
task2 = Abs ("f", tBool) (f :$: tBool :@: f :@: f)
    where
        f = EVar "f"

task2Type = tBool :->: tBool

tests :: [Test]
tests = [ varNotFound
        , functionType
        , polyIdType
        , simpleApplication
        , simpleTypeApplication
        , doubleTypeCheck
        , doubleNat
        , quadrupleTypeCheck
        , selfAppTypeCheck
        ]


main :: IO ()
main = (runTestTT $ TestList tests) >> return ()
