{-# LANGUAGE FlexibleContexts #-}

module Language.Scheme.Evaluator
  ( evalLispVal
  , evalString
  , newEnv
  , runOne
  , withStandardLibrary
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import System.IO

import Paths_scheme

import Language.Scheme.Desugarer
import Language.Scheme.Env
import Language.Scheme.Parser
import Language.Scheme.Primitives
import Language.Scheme.Types

eval :: LispVal -> EvalM LispVal
eval val@(String _) = return val
eval val@(Vector _) = return val
eval val@(Char _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id) = getVar id
eval (List (Atom "begin" : exps)) = last <$> traverse eval exps
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq]) = do
    result <- eval pred
    case result of
      Bool False -> return $ Unspecified
      otherwise -> eval conseq
eval form@(List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False  -> eval alt
        otherwise   -> eval conseq
eval badForm@(List (Atom "if":_)) = throwError $ BadSpecialForm "Unrecognized special form" badForm
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
eval (List [Atom "load", String filename]) = do
    vals <- load filename
    last <$> traverse (desugar' >=> eval) vals
  where
    desugar' = liftThrows . desugar
eval (List (function : args)) = do
    func <- eval function
    argVals <- traverse eval args
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
          evalBody = last <$> traverse eval body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env

load :: String -> EvalM [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

evalLispVal' :: Env -> LispVal -> IOThrowsError LispVal
evalLispVal' env expr =
  let desugar' = liftThrows . desugar
      evalResult = desugar' >=> eval $ expr :: EvalM LispVal
   in runEvalM env evalResult

evalString' :: Env -> String -> IOThrowsError [LispVal]
evalString' env str = case readExprList str of
                        Left e      -> throwError e
                        Right exprs -> traverse (evalLispVal' env) exprs

evalLispVal :: Env -> LispVal -> IO (Either LispError LispVal)
evalLispVal env = runExceptT . (evalLispVal' env)

evalString :: Env -> String -> IO (Either LispError [LispVal])
evalString env = runExceptT . (evalString' env)

runOne :: [String] -> IO ()
runOne args = do
    env <- newEnv >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    withStandardLibrary env $
        (runExceptT $ runEvalM env $ eval (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr . (either show show)

withStandardLibrary :: (MonadIO m) => Env -> m () -> m ()
withStandardLibrary env action = do
    res <- liftIO $ loadStandardLibrary env
    case res of
      Left e  -> liftIO $ putStrLn "Error loading stdlib"
      Right _ -> action
  where loadStandardLibrary env = do
            stdlibPath <- getDataFileName "lib/stdlib.scm"
            evalLispVal env (List [Atom "load", String stdlibPath])

newEnv :: IO Env
newEnv = primitiveBindings
  where
    makeFunc constructor (var, func) = (var, constructor func)

    primitiveBindings :: IO Env
    primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) builtinPrimitives
                                              ++ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)

runEvalM :: Env -> EvalM LispVal -> IOThrowsError LispVal
runEvalM env action = (runReaderT . run) action $ env

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
readAll [String filename] = List <$> load filename

builtinPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
builtinPrimitives = [("apply", applyProc), ("read-all", readAll)]
