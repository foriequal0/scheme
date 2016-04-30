module Scheme.Desugarer
  ( desugar
  ) where

import Control.Monad.Except

import Scheme.Types

desugar :: LispVal -> ThrowsError LispVal
desugar expr@(List (Atom "let": _)) = desugarLet expr
desugar val = return val

desugarLet :: LispVal -> ThrowsError LispVal
desugarLet (List (Atom "let" : (List bindings) : body)) = do
    (params, args) <- collectBindings bindings
    let lambdafiedFunc = List (Atom "lambda" : (List params) : body)
    return $ List (lambdafiedFunc : args)
desugarLet badForm = throwError $ BadSpecialForm "Bad formed let" badForm

collectBindings :: [LispVal] -> ThrowsError ([LispVal], [LispVal])
collectBindings [] = return ([], [])
collectBindings ((List [id@(Atom _), valExpr]):xs) =
    ((id, valExpr) `pairAppend`) <$> collectBindings xs
  where (f, s) `pairAppend` (fx, sx) = (f:fx, s:sx)
collectBindings (binding@(List form):_) =
    throwError $ BadSpecialForm reason binding
  where reason = if length form > 2
                 then "Too many operands in let binding"
                 else "Too few operands in let binding"
collectBindings (x:_) = throwError $ BadSpecialForm "Malformed let" x
