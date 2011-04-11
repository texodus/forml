module Sonnet.AST(
  Definition(..), Axiom(..), Pattern(..), Expression(..), Literal(..), 
  Operator(..), Identifier(..), Type(..)
) where 

import Data.List
import Language.Javascript.JMacro


data Definition = Definition String [Axiom]
                | Struct String Type
                deriving Show

data Axiom      = TypeAxiom Type
                | AssertAxiom [Pattern] Expression
                | RelationalAxiom [Pattern] Expression
                deriving Show

data Pattern    = LiteralPattern Literal
                | IgnorePattern
                | VarPattern String
                | BindPattern String [Pattern]

data Expression = PrefixExpression String [Expression]
                | InfixExpression Expression Operator Expression
                | LetExpression [([Pattern], Expression)] Expression
                | IfExpression Expression Expression Expression
                | LiteralExpression Literal
                | JSExpression JExpr

data Literal    = BooleanLiteral Bool
                | StringLiteral String
                | NumLiteral Double

data Operator   = Operator String

data Identifier = Identifier String
                | ImplicitIdentifier

data Type       = Type String
                | PolymorphicType String [Type]
                | TypeVar String
                | TypeInstance String Int
                | UnknownType Int
                | InvalidType String
                | FunType Type Type
                deriving Eq

instance Show Identifier where
  show (Identifier i) = i

instance Show Pattern where
  show (LiteralPattern lit) = show lit
  show IgnorePattern = "_"
  show (VarPattern i) = i
  show (BindPattern "nil" ps)  = "[]"
  show (BindPattern "cons" ps) = "[" ++ showList' ps ++ "]"
  show (BindPattern i ps) = i ++ "(" ++ concat (intersperse " " (map show ps)) ++ ")"

instance Show Expression where
  show (PrefixExpression "nil" ps)  = "[]"
  show (PrefixExpression "cons" ps) = "[" ++ showList'' ps ++ "]"
  show (PrefixExpression i exs) = i ++ "(" ++ concat (intersperse " " (map show exs)) ++ ")"
  show (InfixExpression ex1 o ex2) = show ex1 ++ " " ++ show o ++ " " ++ show ex2
  show (LetExpression ss ex) = undefined
  show (IfExpression cond ex1 ex2) = "if " ++ show cond ++ " then " ++ show ex1 ++ " else " ++ show ex2
  show (LiteralExpression l) = show l
  show (JSExpression j) = show $ renderJs j

instance Show Literal where
  show (StringLiteral s) = s
  show (NumLiteral n) = show n

instance Show Operator where
  show (Operator s) = s

instance Show Type where
  show (FunType t u) = "(" ++ show t ++ " -> " ++ show u ++ ")"
  show (Type t)      = t
  show (PolymorphicType s t) = s ++ "(" ++ concat (intersperse " " (map show t)) ++ ")"
  show (TypeVar s) =  s
  show (UnknownType i) = "t" ++ show i
  show (InvalidType m) = "TYPE ERROR: " ++ m
  
showList' :: [Pattern] -> String
showList' (x:(BindPattern "cons" y):[]) = show x ++ " " ++ showList' y
showList' (x:_:[]) = show x

showList'' :: [Expression] -> String
showList'' (x:(PrefixExpression "cons" y):[]) = show x ++ " " ++ showList'' y
showList'' (x:_:[]) = show x

