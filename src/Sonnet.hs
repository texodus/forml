{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent.MVar

import System.IO
import System.IO.Unsafe

import Text.ParserCombinators.Parsec hiding ((<|>), many, State)
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
-- Type Checking / Inference

data TypeEquation = TypeEquation Type Type deriving Eq
type TypeEnv      = M.Map String Type

instance Show TypeEquation where
  show (TypeEquation t u) = show t ++ " = " ++ show u
  
genUniqueType :: State Int Type
genUniqueType =  do x <- get
                    put (x + 1)
                    return (UnknownType x)
  
typeCheck :: [Definition] -> [TypeEquation]
typeCheck    defs         =  fst $ runState (typeCheckState defs) 0

typeCheckState :: [Definition] -> State Int [TypeEquation]
typeCheckState    defs
  =  do (symbols, types) <- genInitEnv defs
        eqs <- sequence $ checkDef (mapEnv symbols types) <$> zipWith (,) defs types
        return $ concat eqs

  where genInitEnv []                               = return ([], [])
        genInitEnv (Struct (Identifier i) t:ds)     = do (x, y) <- genInitEnv ds
                                                         return (i : x, t : y)
        genInitEnv (Definition (Identifier i) _:ds) = do utype  <- genUniqueType
                                                         (x, y) <- genInitEnv ds
                                                         return $ (i : x, utype : y)
        mapEnv []     []     = M.empty
        mapEnv (x:xs) (y:ys) = M.insert x y (mapEnv xs ys)

checkDef :: TypeEnv -> (Definition, Type)             -> State Int [TypeEquation]
checkDef    env        ((Definition i axs),        t) =  concat <$> (sequence $ map (checkAxiom env t) axs)
checkDef    env        ((Struct (Identifier i) u), t) =  return [] -- The env already maps i = t directly, no need to add a typeEquation

checkAxiom :: TypeEnv -> Type -> Axiom                  -> State Int [TypeEquation]
checkAxiom    env        t       (TypeAxiom u)          =  return [TypeEquation t u]
checkAxiom    env        t       (AssertAxiom _ _)      =  return []
checkAxiom    env        t       (RelationalAxiom ps e) 
  =  do newEnv     <- augmentEnv env ps
        resultType <- genUniqueType
        funType    <- genFunType newEnv ps resultType
        (TypeEquation t funType :) <$> checkExpression newEnv e resultType
                                                              
genFunType :: TypeEnv -> [Pattern]                   -> Type  -> State Int Type
genFunType    env        []                             t     =  return t
genFunType    env        (LiteralPattern (NumLiteral i):ps) t =  FunType (Type "Num") <$> genFunType env ps t
genFunType    env        (LiteralPattern (StringLiteral i):ps) t =  FunType (Type "String") <$> genFunType env ps t
genFunType    env        (IgnorePattern:ps) t =  do utype <- genUniqueType
                                                    FunType utype <$> genFunType env ps t
genFunType    env        (VarPattern (Identifier i):ps) t     
  =  case M.lookup i env of
        Nothing -> return $ InvalidType $ "Symbol " ++ i ++ " is not defined (This is impossible, augmentEnv should have populated me!)"
        Just x  -> FunType x <$> genFunType env ps t
genFunType    env        (BindPattern (Identifier i) ps:pss) t
  =  case M.lookup i env of
        Nothing -> return $ InvalidType $ "Symbol " ++ i ++ " is not defined (This is impossible, augmentEnv should have populated me!)" 
        Just x  -> FunType (getResultType x) <$> genFunType env pss t
        
getResultType (FunType x y) = getResultType y
getResultType x = x


augmentEnv :: TypeEnv -> [Pattern]                           -> State Int TypeEnv
augmentEnv    env        []                                  =  return env
augmentEnv    env        (LiteralPattern lit:ps)             =  augmentEnv env ps
augmentEnv    env        (IgnorePattern:ps)                  =  augmentEnv env ps
augmentEnv    env        (VarPattern (Identifier i):ps)      =  do utype <- genUniqueType
                                                                   augmentEnv (M.insert i utype env) ps
augmentEnv    env        (BindPattern (Identifier i) ps:pss) =  do augmentEnv env (ps ++ pss)

checkExpression :: TypeEnv -> Expression                         -> Type -> State Int [TypeEquation]
checkExpression    env        (JSExpression _)                      t    =  return [TypeEquation t (Type "IO")]
checkExpression    env        (LiteralExpression (StringLiteral _)) t    =  return [TypeEquation t (Type "String")]
checkExpression    env        (LiteralExpression (NumLiteral _))    t    =  return [TypeEquation t (Type "Num")]
checkExpression    env        (PrefixExpression  (Identifier i) exs) t
  =  do resultType   <- genUniqueType
        argTypes     <- sequence (take (length exs) (repeat genUniqueType))
        functionType <- return $  case M.lookup i env of
          Nothing -> (InvalidType $ "Symbol " ++ i ++ " is not defined")
          Just x  -> x
        ([ TypeEquation t resultType, TypeEquation (genFunType' $ argTypes ++ [resultType]) functionType ] ++) 
          <$> concat <$> sequence [checkExpression env x y | (x, y) <- zipWith (,) exs argTypes ]
checkExpression    env        (InfixExpression ex1 (Operator o) ex2) t
  =  do resultType   <- genUniqueType
        t1 <- genUniqueType
        t2 <- genUniqueType
        functionType <- return $ case o of
                                   "-" -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
                                   "+" -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
                                   "==" -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
                                   "/" -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
                                   "*" -> FunType (Type "Num") (FunType (Type "Num") (Type "Num"))
                                   "|" -> FunType (Type "Bool") (FunType (Type "Bool") (Type "Bool"))
                                   "&" -> FunType (Type "Bool") (FunType (Type "Bool") (Type "Bool"))
        ([ TypeEquation t resultType, TypeEquation (genFunType' $ [t1, t2] ++ [resultType]) functionType ] ++) 
          <$> concat <$> sequence [checkExpression env ex1 t1, checkExpression env ex2 t2]

genFunType' (x:[]) = x 
genFunType' (x:xs) = FunType x (genFunType' xs)


data Substitution = Substitution Type Type 
                  deriving (Eq, Show)

unify :: [TypeEquation] -> [TypeEquation]
unify    eqs            = let ans = applySubs (genSubs eqs) eqs 
                          in  case ans /= eqs of
                              True  -> unify ans
                              False -> ans
                                       
  where genSubs    eqs = concat $ map genSub eqs
        
        genSub (TypeEquation i@(UnknownType _) y)          = [Substitution i y]
        genSub (TypeEquation x@(Type _) i@(UnknownType _)) = [Substitution i x]
        genSub (TypeEquation (FunType x y) (FunType a b))  = Substitution x a : genSub (TypeEquation y b)
        genSub _ = []
        
        applySubs    [] ts = ts        
        applySubs    (s:ss) ts = applySubs ss (concat $ map (applySub s) ts)

        applySub    (Substitution a b) (TypeEquation x y) | a == x = [TypeEquation x (merge b y)] ++ newRules b y
        applySub    sub (TypeEquation t y) = [TypeEquation t (applySubType sub y)]                              
        
        applySubType    (Substitution x y) z | x == z = y
        applySubType    s (FunType a b) = FunType (applySubType s a) (applySubType s b)
        applySubType    _ y = y                
        
--        newRules    _ (UnknownType _)            = []
        newRules    y@(UnknownType _) x | x /= y = [TypeEquation y x] 
        newRules    (Type x) (UnknownType y)     = [TypeEquation (UnknownType y) (Type x)]
        newRules    (FunType a b) (FunType x y)  = (newRules a x) ++ (newRules b y)
        newRules _ _ = []

        merge    (UnknownType _) x =  x
        merge    (FunType a b) (FunType x y) = FunType (merge a x) (merge b y)
        merge    x (UnknownType y)    = x
        merge    (Type x) (Type y) | x == y = Type y
        merge    x@(InvalidType _) _ = x
        merge    _ x@(InvalidType _) = x
        merge    x y = InvalidType $ "Could not resolve types " ++ show x ++ " with " ++ show y

verify :: [TypeEquation] -> Either [String] [String]
verify    eqs = let errors   = [ x | x@(TypeEquation _ t) <- eqs, isValid t ]
                    warnings = [ x | x@(TypeEquation _ t) <- eqs, isWarning t ]
                in case length errors of
                  0 -> Right $ map ((++"\n") . show) warnings
                  n -> Left  $ map ((++"\n") . show) errors 
                  
isWarning (UnknownType _) = True
isWarning (FunType x y) = isWarning x || isWarning y
isWarning _ = False

isValid (InvalidType _) = True
isValid (FunType x y) = isValid x || isValid y
isValid _ = False



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
                deriving Eq
                         
instance Show Type where
  show (FunType t u) = "(" ++ show t ++ " -> " ++ show u ++ ")"
  show (Type t)      = t
  show (UnknownType i) = "t" ++ show i
  show (InvalidType m) = "TYPE ERROR: " ++ m

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
main  = do RunConfig inputs output runMode <- parseArgs <$>getArgs
           let name = head inputs
           hFile  <- openFile name ReadMode
           src    <- trim <$> hGetContents hFile
           case parse cleanComments "Cleaning Comments" src of
             Left ex -> putStrLn $ show ex
             Right s -> parseSyntax s output runMode
                        
  where parseSyntax s output runMode = case parse sonnetP "Parsing Syntax" s of
                          Left ex -> putStrLn $ "\"" ++ show ex ++ "\n" ++ s ++ "\""
                          Right v -> let code = show $ renderJs $ render $ resolveScope v
                                         eqs1 = typeCheck $ resolveScope v
                                         eqs  = unify eqs1
                                     in  case verify eqs of
                                       Right x -> do putStrLn (concat x)
                                                     case runMode of
                                                       Compile -> writeFile output code
                                                       _______ -> do putStrLn $ concat (map ((++ "\n") . show) eqs1)
                                                                     putStrLn $ concat (map ((++ "\n") . show) eqs)
                                       Left x  -> putStrLn (concat x)
                        
data RunMode   = Compile | JustTypeCheck 
data RunConfig = RunConfig [String] String RunMode

parseArgs :: [String] -> RunConfig
parseArgs    args = fst $ runState argsParser args

  where argsParser = do args <- get
                        case args of
                          []     -> return $ RunConfig [] "default.js" Compile
                          (x:xs) -> do put xs
                                       case x of
                                         ('-':'t':[]) -> do RunConfig a b _ <- argsParser
                                                            return $ RunConfig (x:a) b JustTypeCheck
                                         ('-':'o':[]) -> do (name:ys) <- get
                                                            put ys
                                                            RunConfig a _ c <- argsParser
                                                            return $ RunConfig (x:a) name c                                                            
                                         ('-':_) -> error "Could not parse options"
                                         _ -> do RunConfig a b c <- argsParser
                                                 return $ RunConfig (x:a) b c
                        




--------------------------------------------------------------------------------
----
---- Util

trim :: String -> String
trim  = f . f where f = reverse . dropWhile isSpace
                    
csv :: Show a => [a] -> String
csv =  concat . intersperse " " . map show

lg :: (Show a) => a -> a
lg a =  unsafePerformIO $ putStrLn (show a) >> return a
