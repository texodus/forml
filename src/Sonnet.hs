{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Monad
import System.IO
import System.IO.Unsafe

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import System.Environment
import Language.Javascript.JMacro

import Data.Char
import Data.List
import Data.Monoid

import qualified Data.Map as M


--------------------------------------------------------------------------------
--
-- Backend

render :: [Definition] -> JStat
render defs 
  = [$jmacro| var !sonnet = (function() {
                  `(include)`
                  var !tests = [];
                  var !sonnet = {}; 
                  `(foldl1 mappend $ map renderDef defs)`;
                  return sonnet;
              })(); |]

renderDef :: Definition -> JStat
renderDef (Definition (Identifier n) as) = define n $ renderAxioms n as
renderDef (Struct (Identifier i) t)      = define i $ renderRecord i t

define :: String -> JExpr -> JStat
define    name      setBody
  = [$jmacro| sonnet[`(name)`] = `(setBody)`; |]

renderRecord :: String -> Type -> JExpr
renderRecord i t = [$jmacroE| `(renderRecord' i 0 t)` |]

renderRecord' :: String -> Int -> Type -> JExpr
renderRecord' i n (Type t) = [$jmacroE| (function() { var x = []; x.type = `(i)`; return x; })() |]     
renderRecord' i n (FunType t u) 
  = [$jmacroE| (function(arg) {
                   var x = `(renderRecord' i (n + 1) u)`;
                   if (typeof x === "function") {
                       return function(arg2) {
                           var y = x(arg2);
                           y[`(n)`] = arg;
                           return y;
                       };
                   } else {
                       x[`(n)`] = arg;
                       return x;
                   };      
               }) |]

renderAxioms :: String -> [Axiom] -> JExpr
renderAxioms n axs  
  = [$jmacroE| (function() {
                   var !axioms = [];
                   var !argLength = 0;
                   var !name      = `(n)`;                  
                   `(foldl1 mappend (map renderAxiom (reverse axs)))`;
                   if (axioms.length === 1) {
                       return axioms[0];
                   } else {
                       return wrap(tryEach(axioms), argLength);
                   };
               })() |]

renderAxiom :: Axiom -> JStat
renderAxiom (TypeAxiom t) = mempty
renderAxiom (RelationalAxiom patterns expression) 
  = [$jmacro| argLength = `(length patterns)`;
              axioms.push(`(renderPatterns patterns expression)`) |]

renderAxiom ax@(AssertAxiom patterns expression)
  = [$jmacro| tests.push(function() { 
                       var cxt = {};
                       var actual = unwrap(sonnet[name], `(toJArray $ map (renderEvalPattern cxt) (reverse patterns))`);
                       var expected = `(renderExpression cxt expression)`;
                       if (!_.isEqual(actual, expected)) {
                           console.error("FAILED: " ++ name ++ " : " ++ `(show ax)`
                               ++ "\n" ++ "Expected: " ++ expected ++ "\nActual:   " ++ actual);
                           return false;
                       };
                       return true;
                   }); |]

renderEvalPattern :: JExpr -> Pattern -> JExpr
renderEvalPattern cxt (LiteralPattern lit)
  = [$jmacroE| `(renderLiteral lit)` |]

renderEvalPattern cxt (VarPattern (Identifier i))
  = [$jmacroE| `(cxt)`[`(i)`] || sonnet[`(i)`] |]

renderEvalPattern cxt (BindPattern (Identifier i) ps)
  = [$jmacroE| unwrap(`(cxt)`[`(i)`] || sonnet[`(i)`], `(toJArray $ map (renderEvalPattern cxt) (reverse ps))`) |]


   
renderPatterns :: [Pattern] -> Expression -> JExpr
renderPatterns [] e = renderExpression [$jmacroE| {} |] e
renderPatterns ps e  
  = [$jmacroE| function(arg) { 
                   var cxt = {};
                   return `(renderPatterns' cxt ps e)`(arg);
               } |]
renderPatterns' cxt [] e = renderExpression cxt e
    
renderPatterns' cxt (IgnorePattern:ps) e         
  = [$jmacroE| function() { return `(renderPatterns' cxt ps e)` } |]

renderPatterns' cxt (LiteralPattern lit:ps) e
  = [$jmacroE| function(arg) {
                   if (arg == `(renderLiteral lit)`) {
                       return `(renderPatterns' cxt ps e)`;
                   } else {
                       return `(renderStub ps)`;
                   };
               } |]

renderPatterns' cxt (VarPattern (Identifier i):ps) e
  = [$jmacroE| function(arg) {
                   `(cxt)`[`(i)`] = arg;
                   return `(renderPatterns' cxt ps e)`;
               } |]

renderPatterns' cxt (BindPattern (Identifier i) ps:others) e
  = [$jmacroE| function(arg) { 
                   if (arg && arg.type === `(i)`) {
                       var __cxt = unwrap(`(renderPatterns' cxt ps (JSExpression cxt))`, arg);
                       return `(renderPatterns' __cxt others e)`; 
                   } else {
                       return `(renderStub others)`;
                   }
               } |]
    
renderStub [] = [$jmacroE| undefined |]
renderStub (_:xs) = [$jmacroE| function() { return `(renderStub xs)` } |]
        
renderLiteral :: Literal           -> JExpr                        
renderLiteral    (BooleanLiteral b) =  [$jmacroE| `(map toLower $ show b)` |]
renderLiteral    (StringLiteral s)  =  [$jmacroE| `(s)` |]
renderLiteral    (NumLiteral n)     =  [$jmacroE| `(n)` |]

renderExpression :: JExpr -> Expression -> JExpr
renderExpression cxt (LiteralExpression l) = renderLiteral l
renderExpression cxt (JSExpression js) = [$jmacroE| (typeof `(js)` == "function") ? `(js)`(`(cxt)`) : `(js)` |]

renderExpression cxt (PrefixExpression ImplicitIdentifier []) = [$jmacroE| undefined |]
renderExpression cxt (PrefixExpression ImplicitIdentifier (x:_)) = [$jmacroE| `(renderExpression cxt x)` |]

renderExpression cxt (PrefixExpression (Identifier i) []) = [$jmacroE| `(cxt)`[`(i)`] || sonnet[`(i)`] |]
renderExpression cxt (PrefixExpression (Identifier i) exs)
  = [$jmacroE| unwrap(`(cxt)`[`(i)`] || sonnet[`(i)`], `(toJArray $ map (renderExpression cxt) (reverse exs))`) |]

renderExpression cxt (InfixExpression e (Operator i) f) = renderOp i e f
  where renderOp "+"  e f = [$jmacroE| (`(renderExpression cxt e)` +   `(renderExpression cxt f)`) |]
        renderOp "-"  e f = [$jmacroE| (`(renderExpression cxt e)` -   `(renderExpression cxt f)`) |]
        renderOp "/"  e f = [$jmacroE| (`(renderExpression cxt e)` /   `(renderExpression cxt f)`) |]
        renderOp "*"  e f = [$jmacroE| (`(renderExpression cxt e)` *   `(renderExpression cxt f)`) |]
        renderOp "&"  e f = [$jmacroE| (`(renderExpression cxt e)` &&  `(renderExpression cxt f)`) |]
        renderOp "|"  e f = [$jmacroE| (`(renderExpression cxt e)` ||  `(renderExpression cxt f)`) |]
        renderOp "==" e f = [$jmacroE| (`(renderExpression cxt e)` === `(renderExpression cxt f)`) |]

renderExpression cxt (LetExpression [] e) = renderExpression cxt e
renderExpression cxt (LetExpression ((FunctionStatement ImplicitIdentifier ps e):ss) ee)
  = [$jmacroE| (function() { 
                   unwrap(`(renderPatterns ps e)`, `(renderExpression cxt e)`) 
                   return `(renderExpression cxt ((LetExpression ss) ee))`
               })() |]

renderExpression cxt (IfExpression e f g) 
  = [$jmacroE| (function() {
                   if (`(renderExpression cxt e)`) {
                       return `(renderExpression cxt f)`;       
                   } else {
                       return `(renderExpression cxt g)`;
                   };
                })() |]


toJArray :: [JExpr] -> JExpr
toJArray    []       = [$jmacroE| [] |] 
toJArray    (e:es)   = [$jmacroE| (function() { var x = `(toJArray es)`; x.push(`(e)`); return x; })() |]

include :: JStat
include =                  
    [$jmacro| // tries a list of functions in order until ones matches
              var !tryEach = function(axioms) { return function(args) {
                  var axiom = undefined;
                      var n = 0;
                      while(n < axioms.length && axiom === undefined) {
                          axiom = unwrap(axioms[n], args);
                          n++;
                      };
                      return axiom;
                   }; 
               };

               // logs a function's result
               var !id = function(x) { console.log(x); return x };
               
               // applies a curried function to a list of arguments
               var !unwrap = function(f, args) {
                   for(var arg in args) {
                       if (arg != "type") {
                           f = f(args[arg]); 
                       };
                   };
                   return f;
               };

               var runTests = function() {
                 var failed = 0;
                 _.each(tests, function(test) {
                     if (!test()) {
                         failed++;
                     }
                 });
                 console.log(tests.length ++ " tests, " ++ failed ++ " failed");
               };

               $(runTests);

               // converts a function expecting n arguments to a curried function
               var !wrap = function(f, n) {
                   var newF = function(args) { return function(arg) {
                       args.push(arg)
                       return f(args);
                   }; };
                   while (n > 1) {
                       newF = (function(oldF) {
                           return function(args) { return function(arg) {
                               args.push(arg);
                               return oldF(args);
                           }; };
                      })(newF);
                      n = n - 1;
                   };
                   return function(arg) { return newF([])(arg); };
               }; |]



--------------------------------------------------------------------------------
--
-- Type Checking

data TypeEquation = TypeEquation Type Type 
data TypeEnvironment = TypeEnvironment Int (M.Map String Type) deriving Show

instance Show TypeEquation where
  show (TypeEquation t u) = show t ++ " = " ++ show u

typeCheck :: [Definition] -> [TypeEquation]
typeCheck    defs          = let env@(TypeEnvironment _ m) = generateEnvironment defs
                                 eqs = (checkDef env) <$> defs <*> M.elems m
                             in  concat eqs

  where generateEnvironment []                               = TypeEnvironment 0 M.empty
        generateEnvironment (Struct (Identifier i) t:ds)     = generateEnvironment ds
        generateEnvironment (Definition (Identifier i) _:ds) = let TypeEnvironment x m = generateEnvironment ds
                                                               in  TypeEnvironment (x + 1) (M.insert i (UnknownType x) m)

checkDef :: TypeEnvironment -> Definition      -> Type     -> [TypeEquation]
checkDef    env                (Definition _ axs) t        =  concat $ map (checkAxiom env t) axs
checkDef    env                (Struct (Identifier i) u) t =  [TypeEquation t u]

checkAxiom :: TypeEnvironment -> Type -> Axiom                  -> [TypeEquation]
checkAxiom    env                t       (RelationalAxiom ps e) =  let (f, l) = genFunType env ps
                                                                       (TypeEnvironment s m) = (addEnv ps env)
                                                                       newEnv = TypeEnvironment (s + 1) m
                                                                   in  TypeEquation t f : checkExpression newEnv e l
checkAxiom    env                t       (TypeAxiom u)          =  [TypeEquation t u]
checkAxiom    env                t       (AssertAxiom _ _)      =  []

genFunType :: TypeEnvironment    -> [Pattern]                      -> (Type, Type)
genFunType    (TypeEnvironment s m) []                             =  (UnknownType s, UnknownType s)
genFunType    (TypeEnvironment s m) (VarPattern (Identifier i):ts) =  let (f, l) = genFunType (TypeEnvironment (s + 1) m) ts
                                                                      in  (FunType (UnknownType s) f, l)

genFunType' (x:[]) = x 
genFunType' (x:xs) = FunType x (genFunType' xs)

addEnv :: [Pattern]                   -> TypeEnvironment       -> TypeEnvironment
addEnv    []                             x                     =  x
addEnv    (VarPattern (Identifier i):ps) (TypeEnvironment s m) =  addEnv ps (TypeEnvironment (s + 1) (M.insert i (UnknownType s) m))
addEnv    (BindPattern i ps:pss)         env                   =  undefined

checkExpression :: TypeEnvironment -> Expression -> Type -> [TypeEquation]
checkExpression    (TypeEnvironment s m) (PrefixExpression (Identifier i) exs) t = let t1 = UnknownType s
                                                                                       ts = map UnknownType [s + 1 .. s + length exs]
                                                                                   in  TypeEquation t t1 :
                                                                                       concat (checkExpression (TypeEnvironment (s + length exs) m) <$> exs <*> ts)
                                                                                       ++ case M.lookup i m of
                                                                                            Nothing -> [TypeEquation t (InvalidType "TYPE ERROR")]
                                                                                            Just x  -> [TypeEquation t x]
checkExpression    env (InfixExpression ex1 (Operator i) e2) t = undefined
checkExpression    env (LetExpression ss ex) t = undefined
checkExpression    (TypeEnvironment s m) (IfExpression cond ex1 ex2) t = let t1 = UnknownType s 
                                                                             t2 = UnknownType (s + 1)
                                                                             t3 = UnknownType (s + 2)
                                                                             newEnv = TypeEnvironment (s + 3) m
                                                                         in  [ TypeEquation t1 (Type "Bool"),
                                                                               TypeEquation t2 t3,
                                                                               TypeEquation t t2 ] 
                                                                             ++ checkExpression newEnv cond t1
                                                                             ++ checkExpression newEnv ex1 t2
                                                                             ++ checkExpression newEnv ex2 t3
checkExpression    env (JSExpression _) t = [TypeEquation t (Type "IO")]
checkExpression    env (LiteralExpression (StringLiteral _)) t = [TypeEquation t (Type "String")]
checkExpression    env (LiteralExpression (NumLiteral _)) t = [TypeEquation t (Type "Num")]



-- data Expression = PrefixExpression Identifier [Expression]
--                 | InfixExpression Expression Operator Expression
--                 | LetExpression [Statement] Expression
--                 | IfExpression Expression Expression Expression
--                 | LiteralExpression Literal
--                 | JSExpression JExpr


-- ...



unify :: [Type]       -> Type -> Type
unify    []              x    =  x


inferLiteral (StringLiteral _) = Type "String"
inferLiteral (NumLiteral _)    = Type "Num"



--------------------------------------------------------------------------------
--
-- Scope Resolution

data Definition = Definition Identifier [Axiom]
                | Struct Identifier Type
                deriving Show
                  
data Axiom      = TypeAxiom Type
                | AssertAxiom [Pattern] Expression
                | RelationalAxiom [Pattern] Expression
                deriving Show
                         
resolveScope :: Script        -> [Definition]
resolveScope    (Script defs)  = snd $ foldl' scope (0, []) defs

  where scope list (_, s@(StructStatement (TypeStatement i t)))    = (fst list, Struct i t : snd list)
        scope list (_, StructStatement _)                          = undefined
        
        scope list (_, EmptyStatement)                             = list
        scope list (_, TypeStatement i t) 
          | length (snd list) == 0 = (0, [Definition i [TypeAxiom t]])
        scope list (_, FunctionStatement i p e) 
          | length (snd list) == 0 = (0, [Definition i [RelationalAxiom p e]])
        
        scope (i, ds) (j, TypeStatement ImplicitIdentifier t) 
           = let (Definition name as) = last ds 
                    in  (j, (take (length ds - 1) ds) ++ [Definition name (TypeAxiom t : as)])
        scope (i, ds) (j, TypeStatement (Identifier s) t)
                   = case last ds of 
                       Definition (Identifier name) as | name == s -> 
                         (j, (take (length ds - 1) ds) ++ [Definition (Identifier name) (TypeAxiom t : as)])
                       _ -> (j, ds ++ [Definition (Identifier s) [TypeAxiom t]])
                       
        scope (i, ds) (j, FunctionStatement ImplicitIdentifier ps e)
          = let (Definition name as) = last ds
                     in  (j, (take (length ds - 1) ds) ++ [Definition name (RelationalAxiom ps e : as)])
        scope (i, ds) (j, FunctionStatement (Identifier s) ps e)
                   = case last ds of
                       Definition (Identifier name) as | name == s ->
                         (j, (take (length ds - 1) ds) 
                             ++ [Definition (Identifier name) (RelationalAxiom ps e : as)])
                       _ -> (j, ds ++ [Definition (Identifier s) [RelationalAxiom ps e]])
                       
        scope (i, ds) (j, TestStatement ImplicitIdentifier ps e)
          = let (Definition name as) = last ds
                     in  (j, (take (length ds - 1) ds) ++ [Definition name (AssertAxiom ps e : as)])
        scope (i, ds) (j, TestStatement (Identifier s) ps e)
                   = case last ds of
                       Definition (Identifier name) as | name == s ->
                         (j, (take (length ds - 1) ds) 
                             ++ [Definition (Identifier name) (AssertAxiom ps e : as)])
                       _ -> (j, ds ++ [Definition (Identifier s) [AssertAxiom ps e]])



--------------------------------------------------------------------------------
--
-- Parser

data Statement  = TypeStatement Identifier Type
                | StructStatement Statement
                | FunctionStatement Identifier [Pattern] Expression
                | TestStatement Identifier [Pattern] Expression
                | EmptyStatement                  
                deriving Show
                  
data Pattern    = LiteralPattern Literal
                | IgnorePattern
                | VarPattern Identifier
                | BindPattern Identifier [Pattern]
                deriving Show

data Expression = PrefixExpression Identifier [Expression]
                | InfixExpression Expression Operator Expression
                | LetExpression [Statement] Expression
                | IfExpression Expression Expression Expression
                | LiteralExpression Literal
                | JSExpression JExpr
                deriving Show
                         
data Literal    = BooleanLiteral Bool
                | StringLiteral String
                | NumLiteral Double
                deriving Show
                         
data Operator   = Operator String
                deriving Show

data Identifier = Identifier String
                | ImplicitIdentifier
                deriving (Show, Eq)

data Type       = Type String 
                | PolymorphicType String
                | UnknownType Int
                | InvalidType String
                | FunType Type Type
                         
instance Show Type where
  show (FunType t u) = show t ++ " -> " ++ show u
  show (Type t)      = t
  show (UnknownType i) = "t" ++ show i
  show (InvalidType m) = "ERROR"

type ScriptLine = (Int, Statement)

newtype Script = Script [ScriptLine]
               deriving Show

---- Structure

cleanComments  = concat <$> many cc
  where cc      = try (comment >> return "") <|> ((:[]) <$> anyChar)
        comment = string "--" >> (anyChar `manyTill` (newline <|> (eof >> return ' ')))

sonnetP  = Script <$> (statementP `sepEndBy` newline) <* spaces <* eof

statementP = try (line structStatementP) 
             <|> try (line typeStatementP) 
             <|> try (line $  functionStatementP FunctionStatement '=') 
             <|> try (line $ functionStatementP TestStatement '?')
             <|> line emptyExpressionP
             
  where line statement = (,) <$> (length <$> many (oneOf " \v\f\t")) <*> statement <* (many $ oneOf " \v\f\t")
                     
emptyExpressionP =  (many $ oneOf " \v\f\t") >> return EmptyStatement
        
--- Statements        

structStatementP  = StructStatement <$> (string "struct " *> spaces *> (snd <$> statementP))

functionStatementP f c = f <$> scopedId <*> option [] (try argumentsP) <* defOperator <*> expressionP

  where s            = many (oneOf " \t\v\f")
        scopedId     = option ImplicitIdentifier identifierP
        argumentsP   = s *> char '(' *> s *> argP `sepEndBy` s <* s <* char ')'
        defOperator  = s *> char c <* s
        argP         = (LiteralPattern <$> literalP) <|> ignorePatternP <|> try bindPatternP <|> varPattern
        
        ignorePatternP = many1 (char '_') >> return IgnorePattern
        bindPatternP = BindPattern <$> identifierP <* char '(' <*> (argP `sepEndBy` s) <* char ')'
        varPattern   = VarPattern <$> identifierP
        
        
typeStatementP  = TypeStatement <$> identifierP <* typeOperatorP <*> typeP
                    
  where typeOperatorP = spaces *> string ":" *> spaces >> return ()
        typeP         = try funTypeP <|> nestedTypeP <|> monoTypeP
        nestedTypeP   = char '(' >> spaces >> typeP <* spaces <* char ')'
        funTypeP      = FunType <$> (nestedTypeP <|> monoTypeP) <* spaces <* string "->" <* spaces <*> typeP
        monoTypeP     = Type <$> symbolP
        
-- Expressions
        
expressionP = do exp <- jsExpressionP
                       <|> try ifExpressionP 
                       <|> try letExpressionP 
                       <|> try prefixExpressionP 
                       <|> symbolExpressionP 
                       <|> literalExpressionP
                 try (infixTail exp) <|> return exp

  where operatorP = Operator <$> many1 (oneOf "><-*^%$|+/?#")
        infixTail exp = do op <- spaces *> operatorP <* spaces
                           exp2 <- expressionP
                           return (InfixExpression exp op exp2)
                           
jsExpressionP     = JSExpression <$> (char '`' *> ((convert . either undefined id . parseJM) <$> (anyChar `manyTill` (char '`'))))

  where  convert expr = [$jmacroE| (function(x) { var !cxt = x; var !ans; `(expr)`; return ans; }) |]
                           
ifExpressionP     = IfExpression <$> (string "if" *> spaces *> expressionP) 
                    <* spaces <* string "then" <* spaces <*> expressionP
                    <* spaces <* string "else" <* spaces <*> expressionP

letExpressionP    = LetExpression <$> (string "let" *> spaces *> (try (functionStatementP FunctionStatement '=') `sepEndBy` spaces))
                    <* spaces <* string "in" <* spaces <*> expressionP
                       
symbolExpressionP = PrefixExpression <$> identifierP <*> return []
        
prefixExpressionP = PrefixExpression <$> (identifierP <|> return ImplicitIdentifier) 
                    <* char '(' <* spaces <*> expressionP `sepBy` spaces <* char ')'

literalExpressionP = LiteralExpression <$> literalP
                     
literalP = stringP <|> booleanP <|> numP
  where stringP  = StringLiteral <$> (char '"' *> manyTill anyChar (char '"'))
        booleanP = BooleanLiteral <$> ((string "True" >> return True) <|> (string "False" >> return False))
        numP     = (NumLiteral . read) <$> ((++) <$> many1 digit <*> (radix <|> return ""))
        radix    = (++) <$> string "." <*> many1 digit
        
identifierP   = Identifier <$>symbolP

symbolP = (:) <$> letter <*> many (alphaNum <|> oneOf "_'")



--------------------------------------------------------------------------------
--
-- Main

main :: IO ()
main  = do name   <- head <$> getArgs
           hFile  <- openFile name ReadMode
           src    <- trim <$> hGetContents hFile
           case parse cleanComments "Cleaning Comments" src of
             Left ex -> putStrLn $ show ex
             Right s -> putStrLn src >> parseSyntax s
  
  where parseSyntax s = case parse sonnetP "Parsing Syntax" s of
                          Left ex -> putStrLn $ "\"" ++ show ex ++ "\n" ++ s ++ "\""
                          Right s -> let code  = show $ renderJs $ render $ resolveScope s
                                         types = ((++ "\n") . show) <$> (typeCheck $ resolveScope s)
                                     in  putStrLn $ code



--------------------------------------------------------------------------------
----
---- Util

trim :: String -> String
trim  = f . f where f = reverse . dropWhile isSpace
                    
csv :: Show a => [a] -> String
csv =  concat . intersperse " " . map show
