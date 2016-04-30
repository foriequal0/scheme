module Main where

import Scheme
import Scheme.Desugarer
import Scheme.Parser
import Test.Hspec

infix 1 `shouldReturnRight`

action `shouldReturnRight` expected = action >>= (`shouldBe` (Right expected))

evalSpec :: Spec
evalSpec =
  describe "evalString" $ do
    it "evaluates a string" $ do
      env <- newEnv
      evalString env "\"foo\"" `shouldReturnRight` String "foo"

    it "evaluates a positive number" $ do
      env <- newEnv
      evalString env "123" `shouldReturnRight` Number 123

    it "evaluates a negative number" $ do
      env <- newEnv
      evalString env "-123" `shouldReturnRight` (Number . negate $ 123)

    it "evaluates an atom" $ do
      env <- newEnv
      evalString env "'foo" `shouldReturnRight` Atom "foo"

    it "evaluates a true value" $ do
      env <- newEnv
      evalString env "#t" `shouldReturnRight` Bool True

    it "evaluates a false value" $ do
      env <- newEnv
      evalString env "#f" `shouldReturnRight` Bool False

    it "evaluates begin expressions sequentially from left to right" $ do
      env <- newEnv
      evalString env "(begin (define x 0) (set! x 5) (+ x 1))"
        `shouldReturnRight` Number 6

    it "defines a variable" $ do
      env <- newEnv
      evalString env "(define x 28)" `shouldReturnRight` Number 28
      evalString env "x" `shouldReturnRight` Number 28

    it "provides type predicates" $ do
      env <- newEnv
      evalString env "(boolean? #f)" `shouldReturnRight` Bool True
      evalString env "(boolean? 0)" `shouldReturnRight` Bool False
      evalString env "(boolean? '())" `shouldReturnRight` Bool False

      evalString env "(pair? '(a . b))" `shouldReturnRight` Bool True
      evalString env "(pair? '(a b c))" `shouldReturnRight` Bool True
      evalString env "(pair? '())" `shouldReturnRight` Bool False

      evalString env "(symbol? 'foo)" `shouldReturnRight` Bool True
      evalString env "(symbol? (car '(a b)))" `shouldReturnRight` Bool True
      evalString env "(symbol? \"bar\")" `shouldReturnRight` Bool False
      evalString env "(symbol? 'nil)" `shouldReturnRight` Bool True
      evalString env "(symbol? '())" `shouldReturnRight` Bool False
      evalString env "(symbol? #f)" `shouldReturnRight` Bool False

      evalString env "(number? 123)" `shouldReturnRight` Bool True
      evalString env "(number? -123)" `shouldReturnRight` Bool True

      evalString env "(string? 'foo)" `shouldReturnRight` Bool False
      evalString env "(string? \"bar\")" `shouldReturnRight` Bool True
      evalString env "(string? 'nil)" `shouldReturnRight` Bool False
      evalString env "(string? '())" `shouldReturnRight` Bool False
      evalString env "(string? #f)" `shouldReturnRight` Bool False

      evalString env "(procedure? car)" `shouldReturnRight` Bool True
      evalString env "(procedure? 'car)" `shouldReturnRight` Bool False
      evalString env "(procedure? (lambda (x) (* x x)))" `shouldReturnRight` Bool True
      evalString env "(procedure? '(lambda (x) (* x x)))" `shouldReturnRight` Bool False

    it "implements string comparison procedures" $ do
      env <- newEnv
      evalString env "(string=? \"foo\" \"foo\")" `shouldReturnRight` Bool True
      evalString env "(string<? \"bar\" \"foo\")" `shouldReturnRight` Bool True
      evalString env "(string>? \"bar\" \"foo\")" `shouldReturnRight` Bool False
      evalString env "(string<? \"foo\" \"foz\")" `shouldReturnRight` Bool True
      evalString env "(string>? \"foo\" \"foz\")" `shouldReturnRight` Bool False
      evalString env "(string<=? \"bar\" \"foo\")" `shouldReturnRight` Bool True
      evalString env "(string>=? \"bar\" \"foo\")" `shouldReturnRight` Bool False
      evalString env "(string<=? \"foo\" \"foz\")" `shouldReturnRight` Bool True
      evalString env "(string>=? \"foo\" \"foz\")" `shouldReturnRight` Bool False
      evalString env "(string<=? \"foo\" \"foo\")" `shouldReturnRight` Bool True
      evalString env "(string>=? \"foo\" \"foo\")" `shouldReturnRight` Bool True

desugarSpec :: Spec
desugarSpec =
  describe "desugarer" $ do
    let readAndDesugar str = readExpr str >>= desugar

    it "desugars let" $ do
      let infix 1 `shouldDesugaredInto`
          sugared `shouldDesugaredInto` desugared =
              (readAndDesugar sugared) `shouldBe` (readExpr desugared)

      "(let ((x 1)) x)" `shouldDesugaredInto` "((lambda (x) x) 1)"
      "(let ((x 1) (y 2)) (+ x y)))" `shouldDesugaredInto` "((lambda (x y) (+ x y)) 1 2)"
      "(let ((x 1)) (print x) (+ x 1)))"
          `shouldDesugaredInto` "((lambda (x) (print x) (+ x 1)) 1)"

    it "throws on malformed let bindings" $ do
      let anyLeft (Right _) = False
          anyLeft (Left _) = True
          shouldFailToDesugar str = readAndDesugar str `shouldSatisfy` anyLeft
      shouldFailToDesugar "(let x x)"
      shouldFailToDesugar "(let (x) x)"
      shouldFailToDesugar "(let ((1 2)) 1)"
      shouldFailToDesugar "(let ((x)) x)"
      shouldFailToDesugar "(let ((x 1 2) x)"

    it "does not desugar" $ do
      let shouldNotDesugar sugared =
              (readAndDesugar sugared) `shouldBe` (readExpr sugared)
      shouldNotDesugar "(lambda (x y) (some x y))"
      shouldNotDesugar "(define x 28)"
      shouldNotDesugar "#t"
      shouldNotDesugar "()"

main = hspec $ do
  evalSpec
  desugarSpec
