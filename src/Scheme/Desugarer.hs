module Scheme.Desugarer
  ( desugar
  ) where

import Scheme.Types

desugar :: LispVal -> ThrowsError LispVal
desugar = return
