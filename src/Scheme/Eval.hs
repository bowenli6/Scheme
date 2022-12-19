{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do xs <- getList v2
     return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quote"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
eval v@(Number _) = return v
    
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
eval (Symbol sym) = 
  do env <- get
     case H.lookup sym env of
        Just val -> return val
        Nothing  -> throwError $ UndefSymbolError sym
                   

-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v

-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote expr

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
      evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
        do v' <- evalQuasi (n+1) v
           return $ Pair (Symbol "quasiquote") (Pair v' Nil)
      evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
        do v' <- evalQuasi (n-1) v
           return $ Pair (Symbol "unquote") (Pair v' Nil)
      evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
      evalQuasi _ v = return v

    -- cond    
    evalList ((Symbol "cond"):exps) = evalCond exps
      where evalCond []     = throwError $ InvalidSpecialForm "cond" expr
            evalCond (x:[]) = 
              do (cond, body) <- getListOf2 x
                 case cond of 
                  Symbol "else" -> eval body
                  _ -> eval cond >>= (\ c -> case c of
                                              Boolean False -> return Void
                                              _             -> eval body) 
            evalCond (x:xs) =
              do (cond, body) <- getListOf2 x
                 case cond of 
                  Symbol "else" -> throwError $ InvalidSpecialForm "cond" expr
                  _ -> eval cond >>= (\ c -> case c of
                                              Boolean False -> evalCond xs
                                              _             -> eval body) 

    -- let
    evalList [Symbol "let", binds, body] = 
      do env <- get 
         bindList <- getList binds
         bindListVals <- mapM getBinding bindList
         modify $ H.union (H.fromList bindListVals)
         res <- eval body
         put env
         return res

    -- lambda
    evalList [Symbol "lambda", args, body] = 
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
         return val

    -- define function
    evalList [Symbol "define", Pair (Symbol fname) args, body] =
      do env <- get
         argList <- getList args
         val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void

    -- define variable
    evalList [Symbol "define", Symbol var, exp] = 
      do env <- get
         val <- eval exp
         modify $ H.insert var val
         return Void

    -- define-macro
    evalList [Symbol "define-macro", Pair (Symbol fname) args, body] =
      do argList <- getList args
         val <- (\argVal -> Macro argVal body) <$> mapM getSym argList
         modify $ H.insert fname val
         return Void


    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) =
      do f <- eval fexpr
         apply f args

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val

  -- Function
apply f@(Func params body clenv) args 
  | length params == length args = 
    do argVals <- mapM eval args
       env <- get
       modify $ H.union (H.union (H.fromList $ zip params argVals) clenv)
       res <- eval body
       put env
       return res 
  | otherwise = throwError $ CannotApply f args


  -- Macro
apply m@(Macro params body) args
  | length params == length args = 
    do env <- get
       modify $ H.union (H.fromList $ zip params args)
       res <- eval body
       put env
       eval res 
  | otherwise = throwError $ CannotApply m args

  -- Primitive
apply (PrimFunc p) args =
  do argVals <- mapM eval args
     p argVals

  -- Other values are not applicable
apply f args = throwError $ CannotApply f args



