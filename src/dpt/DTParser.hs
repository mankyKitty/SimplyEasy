{-# LANGUAGE OverloadedStrings #-}

module DTParser where

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (haskellStyle)

import Data.List
import Data.Function

import Control.Monad.Identity (Identity(..))

import Text.Parsec
import qualified Text.Parsec as P

import Interpreter (Stmt)

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser style
  where idSt = letter <|> P.char '_'
        names = ["out", "let", "assume", "forall"]
        style = haskellStyle { Tok.reservedNames = names
                             , Tok.identStart = idSt
                             }

parseLet e = do Tok.reserved lexer "let"
                x <- Tok.identifier lexer
                Tok.reserved lexer "="
                t <- parseTermIn 0 e
                return (Let x t)

parseAssume = do Tok.reserved lexer "assume"
                 (xs, ts) <- parseBindings False []
                 return (Assume (reverse (zip xs ts)))

parseOut = do Tok.reserved lexer "out"
              x <- option "" (Tok.stringLiteral lexer)
              return (Out x)

parseStmt :: [String] -> CharParser () (Stmt TermIn TermOut)
parseStmt e = parseLet <|> parseAssume <|> parseOut <|> fmap Eval (parseTermIn 0 e)
