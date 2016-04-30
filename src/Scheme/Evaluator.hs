{-# LANGUAGE FlexibleContexts #-}

module Scheme.Evaluator
  ( evalString
  , newEnv
  , runRepl
  , runOne
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import System.Console.Haskeline
import System.IO

import Scheme.Desugarer
import Scheme.Env
import Scheme.Parser
import Scheme.Primitives
import Scheme.Types

-- FIXME: Add let
eval :: LispVal -> EvalM LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id) = getVar id
eval (List (Atom "begin" : exps)) = fmap last $ mapM eval exps
eval (List [Atom "quote", val]) = return val
-- FIXME: Make alt optional
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise -> eval conseq
eval (List [Atom "set!", Atom var, form]) =
    eval form >>= setVar var
eval (List [Atom "define", Atom var, form]) =
    eval form >>= defineVar var
eval (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc params body >>= defineVar var
eval (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs params body >>= defineVar var
eval (List (Atom "lambda" : List params : body)) =
    makeNormalFunc params body
eval (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs params body
eval (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs [] body
eval (List [Atom "load", String filename]) =
    (load filename) >>= (mapM $ liftThrows . desugar) >>= fmap last . mapM eval
eval (List (function : args)) = do
    func <- eval function
    argVals <- mapM eval args
    apply func argVals
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> EvalM LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else do
           env <- (liftIO $ bindVars closure $ zip params args)
           env' <- bindVarArgs varargs env
           local (const env') evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody = fmap last $ mapM eval body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env

load :: String -> EvalM [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

evalString :: Env -> String -> IO (Either LispError LispVal)
evalString env expr =
    let evalResult = (liftThrows $ (readExpr expr >>= desugar)) >>= eval :: EvalM LispVal
    in
        runEvalM env evalResult

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

-- FIXME: Load stdlib.scm before evaluating the program
runOne :: [String] -> IO ()
runOne args = do
    env <- newEnv >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runEvalM env $ eval (List [Atom "load", String (args !! 0)]))
         >>= hPutStrLn stderr . (either show show)

runRepl :: IO ()
runRepl = runInputT defaultSettings replLoop

replLoop :: InputT IO ()
replLoop = do
    env <- liftIO newEnv
    until_ quitPred (getInputLine "Lisp>>> ") (evalAndPrint env . fromJust)
  where
    quitPred Nothing = True
    quitPred (Just "quit") = True
    quitPred _ = False

    evalAndPrint env expr = do
      evalResult <- liftIO $ evalString env expr
      outputStrLn $ either show show evalResult

newEnv :: IO Env
newEnv = primitiveBindings
  where
    makeFunc constructor (var, func) = (var, constructor func)

    primitiveBindings :: IO Env
    primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) builtinPrimitives
                                              ++ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)

runEvalM :: Env -> EvalM LispVal -> IO (Either LispError LispVal)
runEvalM env action = runExceptT ioThrows
    where ioThrows = (runReaderT . run) action $ env

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> EvalM LispVal
makeFunc varargs params body = do
  env <- ask
  return $ Func (map showVal params) varargs body env

makeNormalFunc :: [LispVal] -> [LispVal] -> EvalM LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> [LispVal] -> [LispVal] -> EvalM LispVal
makeVarargs = makeFunc . Just . showVal

applyProc :: [LispVal] -> EvalM LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

readAll :: [LispVal] -> EvalM LispVal
readAll [String filename] = fmap List $ load filename

builtinPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
builtinPrimitives = [("apply", applyProc), ("read-all", readAll)]
