{-# LANGUAGE FlexibleContexts #-}

module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Text.Printf


type TypeVar = String

data Type = Type :->: Type
          | TVar TypeVar
          | ForAll TypeVar Type
          deriving (Show)

infixr :->:


instance Eq Type where
    (==) = go []
        where
            go :: [(TypeVar, TypeVar)] -> Type -> Type -> Bool
            go ctx (ll :->: lr) (rl :->: rr)   = (go ctx ll rl) && (go ctx lr rr)
            go ctx (ForAll vl l) (ForAll vr r) = go ((vl, vr):ctx) l r
            go ctx (TVar vl) (TVar vr)         =  let res = find (\(name, _) -> name == vl) ctx in
                                                        case res of Nothing     -> vl == vr
                                                                    Just (_, n) -> n == vr
            go _ _ _                           = False

type ExprVar = String

data Expr = EVar ExprVar
          | Abs (ExprVar, Type) Expr
          | TAbs TypeVar Expr
          | Expr :@: Expr
          | Expr :$: Type
          deriving (Show, Eq)

infixl :@: -- Application term to term
infixl :$: -- Application type to term


data Assumption = EAs ExprVar Type
                | TAs TypeVar

newtype Context = Context [Assumption]

getTypeFromContext :: ExprVar -> Context -> Maybe Type
getTypeFromContext varName (Context ctx) = go ctx
    where
        go :: [Assumption] -> Maybe Type
        go []     = Nothing
        go ((EAs varName' t):xs) | varName' == varName = Just t
        go (_:xs) = go xs


addEAs :: ExprVar -> Type -> Context -> Context
addEAs varName varType (Context ctx) = Context $ (EAs varName varType):ctx

addTAs :: TypeVar -> Context -> Context
addTAs typeVarName (Context ctx) = Context $ (TAs typeVarName):ctx

emptyContext :: Context
emptyContext = Context []

fromListContext :: [Assumption] -> Context
fromListContext = Context


typeSubst :: TypeVar -> Type -> Type -> Type
typeSubst typeVarName concreteType = go
    where
        go (tl :->: tr)       = go tl :->: go tr
        go t@(TVar name)      = if name == typeVarName then concreteType
                                                       else t
        go t@(ForAll name st) = if name /= typeVarName then ForAll name $ go st
                                                       else t


typeCheck :: ( Monad m
             , MonadReader Context m
             , MonadError String m
             ) => Expr -> m Type
typeCheck (EVar varName) = do
    t <- reader $ getTypeFromContext varName
    case t of Nothing -> throwError $ printf "Unknown variable: %s" varName
              Just t  -> return t

typeCheck (Abs (varName, varType) e) = do
    t <- local (addEAs varName varType) (typeCheck e)
    return $ varType :->: t

typeCheck (TAbs typeVarName e) = do
    t <- local (addTAs typeVarName) (typeCheck e)
    return $ ForAll typeVarName t

typeCheck (el :@: er) = do
    tl <- typeCheck el
    tr <- typeCheck er
    case tl of (argType :->: resultType) | argType == tr -> return resultType
               _                                         -> throwError errorMsg
                   where
                       errorMsg = printf ("Term (%s) :: (%s)\n"    ++
                                          "is not applicable to\n" ++ 
                                          "term (%s) :: (%s)")
                                          (show er) (show tr) (show el) (show tl)

typeCheck (e :$: concreteType) = do
    exprType <- typeCheck e
    case exprType of (ForAll typeVarName t) -> return $ typeSubst typeVarName concreteType t
                     _                      -> throwError errorMsg
                        where
                            errorMsg = printf ("Type (%s)\n"            ++
                                               "is not applicable to\n" ++
                                               "term (%s) :: (%s)\n")
                                               (show concreteType) (show e) (show exprType)


runTypeCheckInContext :: Context -> Expr -> Either String Type
runTypeCheckInContext ctx expr = runExcept $ runReaderT (typeCheck expr) ctx

runTypeCheck :: Expr -> Either String Type
runTypeCheck expr = runTypeCheckInContext emptyContext expr
