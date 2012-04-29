{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Sonnet.Javascript (render) where

import Control.Applicative

import Language.Javascript.JMacro

import Data.Char
import Data.List
import Data.Monoid


import qualified Data.Map as M
import qualified Data.Set as S

import Sonnet.AST

render :: Program -> String
render (Program xs) = show . renderJs . toStat $ xs

instance ToStat [Statement] where
    toStat = foldl1 mappend . map toStat

instance ToStat Statement where
    toStat (TypeStatement d x)     = mempty
    toStat (ExpressionStatement e) = mempty
    toStat (ImportStatement d)     = mempty

    toStat (ModuleStatement ns xs) = 

        [jmacro| `(ns)` = new (function() { 
                     `(xs)`; 
                 })(); |]

    toStat (DefinitionStatement (Definition name as)) = 

        [jmacro| this[`(name)`] = `(as)` |]

instance ToJExpr Namespace where
    toJExpr (Namespace xs) = f (reverse xs)
        where f (y:ys)     = [jmacroE| `(f ys)`[`(y)`] |]
              f []         = [jmacroE| this |]

instance ToJExpr [Axiom] where
    toJExpr [] = [jmacroE| null |]
    toJExpr (TypeAxiom _:xs) = toJExpr xs
    toJExpr xs @ (EqualityAxiom (Match ps _) _ : _) = 

        let num_params = length ps

            conv 0 jexpr = jexpr
            conv n jexpr = [jmacroE| function(y) { args.push(y); return `(conv (n - 1) jexpr)`; } |]

            body [] = [jmacroE| console.log("Pattern match exhausted") |]
            body (EqualityAxiom (Match ps cond) ex : xs) = 

                [jmacroE| (function() {
                             if (`(ps)` && `(cond)`) {
                                 return `(ex)`;
                             } else {
                                 return `(body xs)`;
                             }
                          })() |]

        in  [jmacroE| (function() {
                          var !args = [];
                          var !bindings = {};
                          var !current = 0;
                          return `(conv num_params $ body xs)`;
                      })() |]

-- This instance handles pattern guards
instance ToJExpr (Maybe Expression) where
    toJExpr Nothing  = toJExpr True
    toJExpr (Just x) = toJExpr x

instance ToJExpr Expression where
    toJExpr (LiteralExpression l) = toJExpr l
    toJExpr (ApplyExpression (SymbolExpression "+") [x, y]) = [jmacroE| `(x)` + `(y)` |]
    toJExpr (SymbolExpression x) = [jmacroE| bindings[`(x)`] |]
    toJExpr x = error $ show x

instance ToJExpr [Pattern] where
    toJExpr [] = toJExpr True
    toJExpr (x:[]) = toJExpr x
    toJExpr (x:xs) = [jmacroE| `(x)` && (function() { current++; return `(xs)`; })() |]

instance ToJExpr Pattern where
    toJExpr AnyPattern = toJExpr True
    toJExpr (VarPattern x) = [jmacroE| (function() { bindings[`(x)`] = args[current]; return true; })() |]

instance ToJExpr Literal where
    toJExpr (StringLiteral s) = toJExpr s
    toJExpr (IntLiteral s)    = toJExpr s
    toJExpr (DoubleLiteral s) = toJExpr s



--------------------------------------------------------------------------------
--
-- Test Backend

-- renderTests :: String -> [Definition] -> String
-- renderTests name      defs
--   = show $ renderJs [$jmacro| describe(`(name)` + " test suite", function() {
--                   `(include)`;
--                   `(foldl1 mappend $ map renderTest defs)`;
--               }); |]

-- renderTest :: Definition -> JStat
-- renderTest    (Definition i as)
--   = [$jmacro| describe(`(i)`, function() {
--                   `(foldl1 mappend $ map (renderTestAxiom i) as)`;
--               }); |]
-- renderTest    _ = mempty

-- renderTestAxiom :: String -> Axiom -> JStat
-- renderTestAxiom name (AssertAxiom ps ex)
--  = [$jmacro| it(`(name)` + "(" + `(concat (intersperse " " (map show ps)))` + ") == " + `(show ex)`, function() {
--                  var cxt = {};
--                  var actual   = unwrap(sonnet[`(name)`], `(toJArray $ map (renderEvalPattern cxt) (reverse ps))`);
--                  var expected = `(renderExpression cxt ex)`;
--                  expect(actual).toEqual(expected);
--              }); |]

-- renderTestAxiom _ _ = mempty



--------------------------------------------------------------------------------
--
-- Backend

-- render :: [Definition] -> String
-- render defs
--   = show $ renderJs [$jmacro| var !sonnet = (function() {
--                   `(include)`
--                   var !sonnet = {};
--                   `(foldl1 mappend $ map renderDef defs)`;
--                   return sonnet;
--               })(); |]

-- renderDef :: Definition -> JStat
-- renderDef (Definition n as) = define n $ renderAxioms n as
-- renderDef (Struct i t)      = define i $ renderRecord i t

-- define :: String -> JExpr -> JStat
-- define    name      setBody
--   = [$jmacro| sonnet[`(name)`] = `(setBody)`; |]

-- renderRecord :: String -> Type -> JExpr
-- renderRecord i t = [$jmacroE| `(renderRecord' i 0 t)` |]

-- renderRecord' :: String -> Int -> Type -> JExpr
-- renderRecord' i n (Type t) = [$jmacroE| (function() { var x = []; x.type = `(i)`; return x; })() |]
-- renderRecord' i n (FunType t u)
--   = [$jmacroE| (function(arg) {
--                    var x = `(renderRecord' i (n + 1) u)`;
--                    if (typeof x === "function") {
--                        return function(arg2) {
--                            var y = x(arg2);
--                            y[`(n)`] = arg;
--                            return y;
--                        };
--                    } else {
--                        x[`(n)`] = arg;
--                        return x;
--                    };
--                }) |]

-- renderAxioms :: String -> [Axiom] -> JExpr
-- renderAxioms n axs
--   = [$jmacroE| (function() {
--                    var !axioms = [];
--                    var !argLength = 0;
--                    var !name      = `(n)`;
--                    `(foldl1 mappend (map renderAxiom (reverse axs)))`;
--                    if (axioms.length === 1) {
--                        return axioms[0];
--                    } else {
--                        return wrap(tryEach(axioms), argLength);
--                    };
--                })() |]

-- renderAxiom :: Axiom -> JStat
-- renderAxiom (TypeAxiom t) = mempty
-- renderAxiom (RelationalAxiom patterns expression)
--   = [$jmacro| argLength = `(length patterns)`;
--               axioms.push(`(renderPatterns patterns expression)`) |]

-- renderAxiom ax@(AssertAxiom patterns expression)
--   = mempty

-- renderEvalPattern :: JExpr -> Pattern -> JExpr
-- renderEvalPattern cxt (LiteralPattern lit)
--   = [$jmacroE| `(renderLiteral lit)` |]

-- renderEvalPattern cxt (VarPattern i)
--   = [$jmacroE| `(cxt)`[`(i)`] || sonnet[`(i)`] |]

-- renderEvalPattern cxt (BindPattern i ps)
--   = [$jmacroE| unwrap(`(cxt)`[`(i)`] || sonnet[`(i)`], `(toJArray $ map (renderEvalPattern cxt) (reverse ps))`) |]

-- renderPatterns :: [Pattern] -> Expression -> JExpr
-- renderPatterns [] e = renderExpression [$jmacroE| {} |] e
-- renderPatterns ps e
--   = [$jmacroE| function(arg) {
--                    var cxt = {};
--                    return `(renderPatterns' cxt ps e)`(arg);
--                } |]
-- renderPatterns' cxt [] e = renderExpression cxt e

-- renderPatterns' cxt (IgnorePattern:ps) e
--   = [$jmacroE| function() { return `(renderPatterns' cxt ps e)` } |]

-- renderPatterns' cxt (LiteralPattern lit:ps) e
--   = [$jmacroE| function(arg) {
--                    if (arg == `(renderLiteral lit)`) {
--                        return `(renderPatterns' cxt ps e)`;
--                    } else {
--                        return `(renderStub ps)`;
--                    };
--                } |]

-- renderPatterns' cxt (VarPattern i:ps) e
--   = [$jmacroE| function(arg) {
--                    `(cxt)`[`(i)`] = arg;
--                    return `(renderPatterns' cxt ps e)`;
--                } |]

-- renderPatterns' cxt (BindPattern i ps:others) e
--   = [$jmacroE| function(arg) {
--                    if (arg && arg.type === `(i)`) {
--                        var __cxt = unwrap(`(renderPatterns' cxt ps (JSExpression cxt))`, arg);
--                        return `(renderPatterns' __cxt others e)`;
--                    } else {
--                        return `(renderStub others)`;
--                    }
--                } |]

-- renderStub [] = [$jmacroE| undefined |]
-- renderStub (_:xs) = [$jmacroE| function() { return `(renderStub xs)` } |]

-- renderLiteral :: Literal           -> JExpr
-- renderLiteral    (BooleanLiteral b) =  [$jmacroE| `(map toLower $ show b)` |]
-- renderLiteral    (StringLiteral s)  =  [$jmacroE| `(s)` |]
-- renderLiteral    (NumLiteral n)     =  [$jmacroE| `(n)` |]

-- renderExpression :: JExpr -> Expression -> JExpr
-- renderExpression cxt (LiteralExpression l) = renderLiteral l
-- renderExpression cxt (JSExpression js) = [$jmacroE| (typeof `(js)` == "function") ? `(js)`(`(cxt)`) : `(js)` |]

-- renderExpression cxt (PrefixExpression i [])
--   = [$jmacroE| (function() { if (`(cxt)`.hasOwnProperty(`(i)`)) { return `(cxt)`[`(i)`]; } else { return sonnet[`(i)`]; } })() |]
-- renderExpression cxt (PrefixExpression i exs)
--   = [$jmacroE| unwrap(`(cxt)`[`(i)`] || sonnet[`(i)`], `(toJArray $ map (renderExpression cxt) (reverse exs))`) |]

-- renderExpression cxt (InfixExpression e (Operator i) f) = renderOp i e f
--   where renderOp "+"  e f = [$jmacroE| (`(renderExpression cxt e)` +   `(renderExpression cxt f)`) |]
--         renderOp "-"  e f = [$jmacroE| (`(renderExpression cxt e)` -   `(renderExpression cxt f)`) |]
--         renderOp "/"  e f = [$jmacroE| (`(renderExpression cxt e)` /   `(renderExpression cxt f)`) |]
--         renderOp "*"  e f = [$jmacroE| (`(renderExpression cxt e)` *   `(renderExpression cxt f)`) |]
--         renderOp "&"  e f = [$jmacroE| (`(renderExpression cxt e)` &&  `(renderExpression cxt f)`) |]
--         renderOp "|"  e f = [$jmacroE| (`(renderExpression cxt e)` ||  `(renderExpression cxt f)`) |]
--         renderOp "==" e f = [$jmacroE| (`(renderExpression cxt e)` === `(renderExpression cxt f)`) |]

-- -- renderExpression cxt (LetExpression [] e) = renderExpression cxt e
-- -- renderExpression cxt (LetExpression (FunctionStatement ImplicitIdentifier ps e:ss) ee)
-- --   = [$jmacroE| (function() {
-- --                    unwrap(`(renderPatterns ps e)`, `(renderExpression cxt e)`)
-- --                    return `(renderExpression cxt ((LetExpression ss) ee))`
-- --                })() |]

-- renderExpression cxt (IfExpression e f g)
--   = [$jmacroE| (function() {
--                    if (`(renderExpression cxt e)`) {
--                        return `(renderExpression cxt f)`;
--                    } else {
--                        return `(renderExpression cxt g)`;
--                    };
--                 })() |]


-- toJArray :: [JExpr] -> JExpr
-- toJArray    []       = [$jmacroE| [] |]
-- toJArray    (e:es)   = [$jmacroE| (function() { var x = `(toJArray es)`; x.push(`(e)`); return x; })() |]

-- include :: JStat
-- include =
--     [$jmacro| // tries a list of functions in order until ones matches
--               var !tryEach = function(axioms) { return function(args) {
--                   var axiom = undefined;
--                       var n = 0;
--                       while(n < axioms.length && axiom === undefined) {
--                           axiom = unwrap(axioms[n], args);
--                           n++;
--                       };
--                       return axiom;
--                    };
--                };

--                // logs a function's result
--                var !id = function(x) { console.log(x); return x };

--                // applies a curried function to a list of arguments
--                var !unwrap = function(f, args) {
--                    for(var arg in args) {
--                        if (arg != "type") {
--                            f = f(args[arg]);
--                        };
--                    };
--                    return f;
--                };

--                // converts a function expecting n arguments to a curried function
--                var !wrap = function(f, n) {
--                    var newF = function(args) { return function(arg) {
--                        args.push(arg)
--                        return f(args);
--                    }; };
--                    while (n > 1) {
--                        newF = (function(oldF) {
--                            return function(args) { return function(arg) {
--                                args.push(arg);
--                                return oldF(args);
--                            }; };
--                       })(newF);
--                       n = n - 1;
--                    };
--                    return function(arg) { return newF([])(arg); };
--                }; |]

