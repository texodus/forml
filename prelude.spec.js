var is_array;
is_array = (function(jmId_0)
            {
              return (jmId_0 instanceof Array);
            });
var error;
error = (function(jmId_1)
         {
           throw(jmId_1);
         });
var exhaust;
exhaust = (function()
           {
             error("Pattern Match Exhausted");
           });
var check;
check = (function(jmId_2)
         {
           result = (typeof(jmId_2) != "undefined");
           return result;
         });
describe("prelude",
         (function()
          {
            var object_;
            object_ = prelude["object_"];
            var array_;
            array_ = prelude["array_"];
            var not;
            not = prelude["not"];
            var run;
            run = prelude["run"];
            var _grea_grea_eq;
            _grea_grea_eq = prelude["_grea_grea_eq"];
            var ret;
            ret = prelude["ret"];
            var _grea_grea;
            _grea_grea = prelude["_grea_grea"];
            var log;
            log = prelude["log"];
            var _and_and;
            _and_and = prelude["_and_and"];
            var _or_or;
            _or_or = prelude["_or_or"];
            var _star;
            _star = prelude["_star"];
            var _forw;
            _forw = prelude["_forw"];
            var _plus;
            _plus = prelude["_plus"];
            var _minu;
            _minu = prelude["_minu"];
            var _less_eq;
            _less_eq = prelude["_less_eq"];
            var _grea_eq;
            _grea_eq = prelude["_grea_eq"];
            var _less;
            _less = prelude["_less"];
            var _grea;
            _grea = prelude["_grea"];
            var _eq_eq;
            _eq_eq = prelude["_eq_eq"];
            var _bang_eq;
            _bang_eq = prelude["_bang_eq"];
            var fib;
            fib = prelude["fib"];
            var speedtest;
            speedtest = prelude.speedtest;
            var _less_or;
            _less_or = prelude["_less_or"];
            var _or_grea;
            _or_grea = prelude["_or_grea"];
            var _less_col;
            _less_col = prelude["_less_col"];
            var _comp_col;
            _comp_col = prelude["_comp_col"];
            var id;
            id = prelude["id"];
            var flip;
            flip = prelude["flip"];
            var err;
            err = prelude["err"];
            var option;
            option = prelude["option"];
            var list;
            list = prelude.list;
            var sequence;
            sequence = prelude.sequence;
            var map;
            map = prelude.map;
            var jmId_3;
            jmId_3 = new (function()
                          {
                            it("object_ {  }",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return object_({});
                                            }
                                            catch(jmId_4)
                                            {
                                              return { 'error': jmId_4
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("array_ [  ]",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return array_([]);
                                            }
                                            catch(jmId_5)
                                            {
                                              return { 'error': jmId_5
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("not false",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return not(false);
                                            }
                                            catch(jmId_6)
                                            {
                                              return { 'error': jmId_6
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("not array_ {  }",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return not(array_({}));
                                            }
                                            catch(jmId_7)
                                            {
                                              return { 'error': jmId_7
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("not object_ 0",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return not(object_(0));
                                            }
                                            catch(jmId_8)
                                            {
                                              return { 'error': jmId_8
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("run `(function()\n {\n   return (1 + 4);\n })` >>= (λx = `(function()\n {\n   return (2 + 3);\n })` >>= (λy = log \"x is \" + x + \" and y is \" + y >>= (λ_ = `(function()\n {\n   return (x + y);\n })` >>= (λans = ret 10 == ans))))",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return run(_grea_grea_eq((function()
                                                                        {
                                                                          return (1 + 4);
                                                                        }))((function()
                                                                             {
                                                                               return (function(__a__)
                                                                                       {
                                                                                         var x;
                                                                                         x = null;
                                                                                         if((((function()
                                                                                               {
                                                                                                 if((typeof(__a__)
                                                                                                     !=
                                                                                                     "undefined"))
                                                                                                 {
                                                                                                   x = __a__;
                                                                                                   return true;
                                                                                                 }
                                                                                                 else
                                                                                                 {
                                                                                                   return false;
                                                                                                 };
                                                                                               })()
                                                                                              &&
                                                                                              true)
                                                                                             &&
                                                                                             true))
                                                                                         {
                                                                                           return _grea_grea_eq((function()
                                                                                                                 {
                                                                                                                   return (2
                                                                                                                           +
                                                                                                                           3);
                                                                                                                 }))((function()
                                                                                                                      {
                                                                                                                        return (function(__a__)
                                                                                                                                {
                                                                                                                                  var y;
                                                                                                                                  y = null;
                                                                                                                                  if((((function()
                                                                                                                                        {
                                                                                                                                          if((typeof(__a__)
                                                                                                                                              !=
                                                                                                                                              "undefined"))
                                                                                                                                          {
                                                                                                                                            y = __a__;
                                                                                                                                            return true;
                                                                                                                                          }
                                                                                                                                          else
                                                                                                                                          {
                                                                                                                                            return false;
                                                                                                                                          };
                                                                                                                                        })()
                                                                                                                                       &&
                                                                                                                                       true)
                                                                                                                                      &&
                                                                                                                                      true))
                                                                                                                                  {
                                                                                                                                    return _grea_grea_eq(log(((("x is "
                                                                                                                                                                +
                                                                                                                                                                x)
                                                                                                                                                               +
                                                                                                                                                               " and y is ")
                                                                                                                                                              +
                                                                                                                                                              y)))((function()
                                                                                                                                                                    {
                                                                                                                                                                      return (function(__a__)
                                                                                                                                                                              {
                                                                                                                                                                                if(((true
                                                                                                                                                                                     &&
                                                                                                                                                                                     true)
                                                                                                                                                                                    &&
                                                                                                                                                                                    true))
                                                                                                                                                                                {
                                                                                                                                                                                  return _grea_grea_eq((function()
                                                                                                                                                                                                        {
                                                                                                                                                                                                          return (x
                                                                                                                                                                                                                  +
                                                                                                                                                                                                                  y);
                                                                                                                                                                                                        }))((function()
                                                                                                                                                                                                             {
                                                                                                                                                                                                               return (function(__a__)
                                                                                                                                                                                                                       {
                                                                                                                                                                                                                         var ans;
                                                                                                                                                                                                                         ans = null;
                                                                                                                                                                                                                         if((((function()
                                                                                                                                                                                                                               {
                                                                                                                                                                                                                                 if((typeof(__a__)
                                                                                                                                                                                                                                     !=
                                                                                                                                                                                                                                     "undefined"))
                                                                                                                                                                                                                                 {
                                                                                                                                                                                                                                   ans = __a__;
                                                                                                                                                                                                                                   return true;
                                                                                                                                                                                                                                 }
                                                                                                                                                                                                                                 else
                                                                                                                                                                                                                                 {
                                                                                                                                                                                                                                   return false;
                                                                                                                                                                                                                                 };
                                                                                                                                                                                                                               })()
                                                                                                                                                                                                                              &&
                                                                                                                                                                                                                              true)
                                                                                                                                                                                                                             &&
                                                                                                                                                                                                                             true))
                                                                                                                                                                                                                         {
                                                                                                                                                                                                                           return ret(_eq_eq(10)(ans));
                                                                                                                                                                                                                         }
                                                                                                                                                                                                                         else
                                                                                                                                                                                                                         {
                                                                                                                                                                                                                           args = [];
                                                                                                                                                                                                                           exhaust();
                                                                                                                                                                                                                         };
                                                                                                                                                                                                                       });
                                                                                                                                                                                                             })());
                                                                                                                                                                                }
                                                                                                                                                                                else
                                                                                                                                                                                {
                                                                                                                                                                                  args = [];
                                                                                                                                                                                  exhaust();
                                                                                                                                                                                };
                                                                                                                                                                              });
                                                                                                                                                                    })());
                                                                                                                                  }
                                                                                                                                  else
                                                                                                                                  {
                                                                                                                                    args = [];
                                                                                                                                    exhaust();
                                                                                                                                  };
                                                                                                                                });
                                                                                                                      })());
                                                                                         }
                                                                                         else
                                                                                         {
                                                                                           args = [];
                                                                                           exhaust();
                                                                                         };
                                                                                       });
                                                                             })()));
                                            }
                                            catch(jmId_9)
                                            {
                                              return { 'error': jmId_9
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("run ret 5 == 5",
                               (function()
                                {
                                  expect(run(ret(5))).toEqual(5);
                                }));
                            it("run log \"test\" >> `(function()\n {\n   return (5 + 5);\n })` == 10",
                               (function()
                                {
                                  expect(run(_grea_grea(log("test"))((function()
                                                                      {
                                                                        return (5 + 5);
                                                                      })))).
                                  toEqual(10);
                                }));
                            it("let x  = 0\n| y  = `(function()\n {\n   x = 1;\n })` >>= (λz = ret z) in (x == 0)",
                               (function()
                                {
                                  expect((function()
                                          {
                                            try
                                            {
                                              return (function()
                                                      {
                                                        var x;
                                                        x = (function()
                                                             {
                                                               if((true && true))
                                                               {
                                                                 return 0;
                                                               }
                                                               else
                                                               {
                                                                 args = [];
                                                                 exhaust();
                                                               };
                                                             })();
                                                        var y;
                                                        y = (function()
                                                             {
                                                               if((true && true))
                                                               {
                                                                 return _grea_grea_eq((function()
                                                                                       {
                                                                                         x = 1;
                                                                                       }))((function()
                                                                                            {
                                                                                              return (function(__a__)
                                                                                                      {
                                                                                                        var z;
                                                                                                        z = null;
                                                                                                        if((((function()
                                                                                                              {
                                                                                                                if((typeof(__a__)
                                                                                                                    !=
                                                                                                                    "undefined"))
                                                                                                                {
                                                                                                                  z = __a__;
                                                                                                                  return true;
                                                                                                                }
                                                                                                                else
                                                                                                                {
                                                                                                                  return false;
                                                                                                                };
                                                                                                              })()
                                                                                                             &&
                                                                                                             true)
                                                                                                            &&
                                                                                                            true))
                                                                                                        {
                                                                                                          return ret(z);
                                                                                                        }
                                                                                                        else
                                                                                                        {
                                                                                                          args = [];
                                                                                                          exhaust();
                                                                                                        };
                                                                                                      });
                                                                                            })());
                                                               }
                                                               else
                                                               {
                                                                 args = [];
                                                                 exhaust();
                                                               };
                                                             })();
                                                        return _eq_eq(x)(0);
                                                      })();
                                            }
                                            catch(jmId_10)
                                            {
                                              return { 'error': jmId_10
                                                     };
                                            };
                                          })()).
                                  toEqual(true);
                                }));
                            it("3 * 4 + 5 * 4 == 64 \/ 2",
                               (function()
                                {
                                  expect(((3 * 4) + (5 * 4))).toEqual(_forw(64)(2));
                                }));
                            it("4 - 1 != 5 - 10",
                               (function()
                                {
                                  expect((4 - 1)).toNotEqual((5 - 10));
                                }));
                            it("10 >= 5 + 5 != 4 + 5 <= 10 - 2",
                               (function()
                                {
                                  expect((10 >= (5 + 5))).toNotEqual(((4 + 5) <= (10 - 2)));
                                }));
                            it("{ test = 1 } == { test = 1 } == true",
                               (function()
                                {
                                  expect(_eq_eq({ 'test': 1
                                                })({ 'test': 1
                                                   })).
                                  toEqual(true);
                                }));
                            it("{ test = 1 } != { test = 1 } == false",
                               (function()
                                {
                                  expect(!(_eq_eq({ 'test': 1
                                                  })({ 'test': 1
                                                     }))).
                                  toEqual(false);
                                }));
                            describe("speedtest",
                                     (function()
                                      {
                                        var time;
                                        time = prelude.speedtest["time"];
                                        var fast_fib;
                                        fast_fib = prelude.speedtest["fast_fib"];
                                        var floor;
                                        floor = prelude.speedtest["floor"];
                                        var jmId_11;
                                        jmId_11 = new (function()
                                                       {
                                                         it("fast_fib 7 == fib 7",
                                                            (function()
                                                             {
                                                               expect(fast_fib(7)).toEqual(fib(7));
                                                             }));
                                                         it("run ret <| fast_fib 35 >>= (λ_ = time (λ_ = fast_fib 35) >>= (λfast_time = time (λ_ = fib 35) >>= (λslow_time = let ratio  = floor <| fast_time \/ slow_time * 100 in (log \"Runtime speed ~\" + ratio + \"%\" >>= (λ_ = ret ratio >= 80)))))",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return run(_grea_grea_eq(_less_or(ret)(fast_fib(35)))((function()
                                                                                                                                  {
                                                                                                                                    return (function(__a__)
                                                                                                                                            {
                                                                                                                                              if(((true
                                                                                                                                                   &&
                                                                                                                                                   true)
                                                                                                                                                  &&
                                                                                                                                                  true))
                                                                                                                                              {
                                                                                                                                                return _grea_grea_eq(time((function()
                                                                                                                                                                           {
                                                                                                                                                                             return (function(__a__)
                                                                                                                                                                                     {
                                                                                                                                                                                       if(((true
                                                                                                                                                                                            &&
                                                                                                                                                                                            true)
                                                                                                                                                                                           &&
                                                                                                                                                                                           true))
                                                                                                                                                                                       {
                                                                                                                                                                                         return fast_fib(35);
                                                                                                                                                                                       }
                                                                                                                                                                                       else
                                                                                                                                                                                       {
                                                                                                                                                                                         args = [];
                                                                                                                                                                                         exhaust();
                                                                                                                                                                                       };
                                                                                                                                                                                     });
                                                                                                                                                                           })()))((function()
                                                                                                                                                                                   {
                                                                                                                                                                                     return (function(__a__)
                                                                                                                                                                                             {
                                                                                                                                                                                               var fast_time;
                                                                                                                                                                                               fast_time = null;
                                                                                                                                                                                               if((((function()
                                                                                                                                                                                                     {
                                                                                                                                                                                                       if((typeof(__a__)
                                                                                                                                                                                                           !=
                                                                                                                                                                                                           "undefined"))
                                                                                                                                                                                                       {
                                                                                                                                                                                                         fast_time = __a__;
                                                                                                                                                                                                         return true;
                                                                                                                                                                                                       }
                                                                                                                                                                                                       else
                                                                                                                                                                                                       {
                                                                                                                                                                                                         return false;
                                                                                                                                                                                                       };
                                                                                                                                                                                                     })()
                                                                                                                                                                                                    &&
                                                                                                                                                                                                    true)
                                                                                                                                                                                                   &&
                                                                                                                                                                                                   true))
                                                                                                                                                                                               {
                                                                                                                                                                                                 return _grea_grea_eq(time((function()
                                                                                                                                                                                                                            {
                                                                                                                                                                                                                              return (function(__a__)
                                                                                                                                                                                                                                      {
                                                                                                                                                                                                                                        if(((true
                                                                                                                                                                                                                                             &&
                                                                                                                                                                                                                                             true)
                                                                                                                                                                                                                                            &&
                                                                                                                                                                                                                                            true))
                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                          return fib(35);
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                          args = [];
                                                                                                                                                                                                                                          exhaust();
                                                                                                                                                                                                                                        };
                                                                                                                                                                                                                                      });
                                                                                                                                                                                                                            })()))((function()
                                                                                                                                                                                                                                    {
                                                                                                                                                                                                                                      return (function(__a__)
                                                                                                                                                                                                                                              {
                                                                                                                                                                                                                                                var slow_time;
                                                                                                                                                                                                                                                slow_time = null;
                                                                                                                                                                                                                                                if((((function()
                                                                                                                                                                                                                                                      {
                                                                                                                                                                                                                                                        if((typeof(__a__)
                                                                                                                                                                                                                                                            !=
                                                                                                                                                                                                                                                            "undefined"))
                                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                                          slow_time = __a__;
                                                                                                                                                                                                                                                          return true;
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                        else
                                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                                          return false;
                                                                                                                                                                                                                                                        };
                                                                                                                                                                                                                                                      })()
                                                                                                                                                                                                                                                     &&
                                                                                                                                                                                                                                                     true)
                                                                                                                                                                                                                                                    &&
                                                                                                                                                                                                                                                    true))
                                                                                                                                                                                                                                                {
                                                                                                                                                                                                                                                  return (function()
                                                                                                                                                                                                                                                          {
                                                                                                                                                                                                                                                            var ratio;
                                                                                                                                                                                                                                                            ratio = (function()
                                                                                                                                                                                                                                                                     {
                                                                                                                                                                                                                                                                       if((true
                                                                                                                                                                                                                                                                           &&
                                                                                                                                                                                                                                                                           true))
                                                                                                                                                                                                                                                                       {
                                                                                                                                                                                                                                                                         return _less_or(floor)((_forw(fast_time)(slow_time)
                                                                                                                                                                                                                                                                                                 *
                                                                                                                                                                                                                                                                                                 100));
                                                                                                                                                                                                                                                                       }
                                                                                                                                                                                                                                                                       else
                                                                                                                                                                                                                                                                       {
                                                                                                                                                                                                                                                                         args = [];
                                                                                                                                                                                                                                                                         exhaust();
                                                                                                                                                                                                                                                                       };
                                                                                                                                                                                                                                                                     })();
                                                                                                                                                                                                                                                            return _grea_grea_eq(log((("Runtime speed ~"
                                                                                                                                                                                                                                                                                       +
                                                                                                                                                                                                                                                                                       ratio)
                                                                                                                                                                                                                                                                                      +
                                                                                                                                                                                                                                                                                      "%")))((function()
                                                                                                                                                                                                                                                                                              {
                                                                                                                                                                                                                                                                                                return (function(__a__)
                                                                                                                                                                                                                                                                                                        {
                                                                                                                                                                                                                                                                                                          if(((true
                                                                                                                                                                                                                                                                                                               &&
                                                                                                                                                                                                                                                                                                               true)
                                                                                                                                                                                                                                                                                                              &&
                                                                                                                                                                                                                                                                                                              true))
                                                                                                                                                                                                                                                                                                          {
                                                                                                                                                                                                                                                                                                            return ret((ratio
                                                                                                                                                                                                                                                                                                                        >=
                                                                                                                                                                                                                                                                                                                        80));
                                                                                                                                                                                                                                                                                                          }
                                                                                                                                                                                                                                                                                                          else
                                                                                                                                                                                                                                                                                                          {
                                                                                                                                                                                                                                                                                                            args = [];
                                                                                                                                                                                                                                                                                                            exhaust();
                                                                                                                                                                                                                                                                                                          };
                                                                                                                                                                                                                                                                                                        });
                                                                                                                                                                                                                                                                                              })());
                                                                                                                                                                                                                                                          })();
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                else
                                                                                                                                                                                                                                                {
                                                                                                                                                                                                                                                  args = [];
                                                                                                                                                                                                                                                  exhaust();
                                                                                                                                                                                                                                                };
                                                                                                                                                                                                                                              });
                                                                                                                                                                                                                                    })());
                                                                                                                                                                                               }
                                                                                                                                                                                               else
                                                                                                                                                                                               {
                                                                                                                                                                                                 args = [];
                                                                                                                                                                                                 exhaust();
                                                                                                                                                                                               };
                                                                                                                                                                                             });
                                                                                                                                                                                   })());
                                                                                                                                              }
                                                                                                                                              else
                                                                                                                                              {
                                                                                                                                                args = [];
                                                                                                                                                exhaust();
                                                                                                                                              };
                                                                                                                                            });
                                                                                                                                  })()));
                                                                         }
                                                                         catch(jmId_12)
                                                                         {
                                                                           return { 'error': jmId_12
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                       })();
                                      }));
                            it("3 |> (λy = y + 1) |> (λy = y + 1) == 5",
                               (function()
                                {
                                  expect(_or_grea(_or_grea(3)((function()
                                                               {
                                                                 return (function(__a__)
                                                                         {
                                                                           var y;
                                                                           y = null;
                                                                           if((((function()
                                                                                 {
                                                                                   if((typeof(__a__)
                                                                                       !=
                                                                                       "undefined"))
                                                                                   {
                                                                                     y = __a__;
                                                                                     return true;
                                                                                   }
                                                                                   else
                                                                                   {
                                                                                     return false;
                                                                                   };
                                                                                 })()
                                                                                &&
                                                                                true)
                                                                               &&
                                                                               true))
                                                                           {
                                                                             return (y + 1);
                                                                           }
                                                                           else
                                                                           {
                                                                             args = [];
                                                                             exhaust();
                                                                           };
                                                                         });
                                                               })()))((function()
                                                                       {
                                                                         return (function(__a__)
                                                                                 {
                                                                                   var y;
                                                                                   y = null;
                                                                                   if((((function()
                                                                                         {
                                                                                           if((typeof(__a__)
                                                                                               !=
                                                                                               "undefined"))
                                                                                           {
                                                                                             y = __a__;
                                                                                             return true;
                                                                                           }
                                                                                           else
                                                                                           {
                                                                                             return false;
                                                                                           };
                                                                                         })()
                                                                                        &&
                                                                                        true)
                                                                                       &&
                                                                                       true))
                                                                                   {
                                                                                     return (y + 1);
                                                                                   }
                                                                                   else
                                                                                   {
                                                                                     args = [];
                                                                                     exhaust();
                                                                                   };
                                                                                 });
                                                                       })())).
                                  toEqual(5);
                                }));
                            it("(λy x z = x + y + z + 1) 1 <| 3 <| 4 == 9",
                               (function()
                                {
                                  expect(_less_or(_less_or((function()
                                                            {
                                                              return (function(__c__)
                                                                      {
                                                                        return (function(__b__)
                                                                                {
                                                                                  return (function(__a__)
                                                                                          {
                                                                                            var y;
                                                                                            y = null;
                                                                                            var x;
                                                                                            x = null;
                                                                                            var z;
                                                                                            z = null;
                                                                                            if((((function()
                                                                                                  {
                                                                                                    if((typeof(__c__)
                                                                                                        !=
                                                                                                        "undefined"))
                                                                                                    {
                                                                                                      y = __c__;
                                                                                                      return true;
                                                                                                    }
                                                                                                    else
                                                                                                    {
                                                                                                      return false;
                                                                                                    };
                                                                                                  })()
                                                                                                 &&
                                                                                                 ((function()
                                                                                                   {
                                                                                                     if((typeof(__b__)
                                                                                                         !=
                                                                                                         "undefined"))
                                                                                                     {
                                                                                                       x = __b__;
                                                                                                       return true;
                                                                                                     }
                                                                                                     else
                                                                                                     {
                                                                                                       return false;
                                                                                                     };
                                                                                                   })()
                                                                                                  &&
                                                                                                  ((function()
                                                                                                    {
                                                                                                      if((typeof(__a__)
                                                                                                          !=
                                                                                                          "undefined"))
                                                                                                      {
                                                                                                        z = __a__;
                                                                                                        return true;
                                                                                                      }
                                                                                                      else
                                                                                                      {
                                                                                                        return false;
                                                                                                      };
                                                                                                    })()
                                                                                                   &&
                                                                                                   true)))
                                                                                                &&
                                                                                                true))
                                                                                            {
                                                                                              return (((x
                                                                                                        +
                                                                                                        y)
                                                                                                       +
                                                                                                       z)
                                                                                                      +
                                                                                                      1);
                                                                                            }
                                                                                            else
                                                                                            {
                                                                                              args = [];
                                                                                              exhaust();
                                                                                            };
                                                                                          });
                                                                                });
                                                                      });
                                                            })()(1))(3))(4)).
                                  toEqual(9);
                                }));
                            it("(λx = x - 3) <: (λx = x - 3) <: 5 + 5 == 4",
                               (function()
                                {
                                  expect(_less_col((function()
                                                    {
                                                      return (function(__a__)
                                                              {
                                                                var x;
                                                                x = null;
                                                                if((((function()
                                                                      {
                                                                        if((typeof(__a__)
                                                                            !=
                                                                            "undefined"))
                                                                        {
                                                                          x = __a__;
                                                                          return true;
                                                                        }
                                                                        else
                                                                        {
                                                                          return false;
                                                                        };
                                                                      })()
                                                                     &&
                                                                     true)
                                                                    &&
                                                                    true))
                                                                {
                                                                  return (x - 3);
                                                                }
                                                                else
                                                                {
                                                                  args = [];
                                                                  exhaust();
                                                                };
                                                              });
                                                    })())(_less_col((function()
                                                                     {
                                                                       return (function(__a__)
                                                                               {
                                                                                 var x;
                                                                                 x = null;
                                                                                 if((((function()
                                                                                       {
                                                                                         if((typeof(__a__)
                                                                                             !=
                                                                                             "undefined"))
                                                                                         {
                                                                                           x = __a__;
                                                                                           return true;
                                                                                         }
                                                                                         else
                                                                                         {
                                                                                           return false;
                                                                                         };
                                                                                       })()
                                                                                      &&
                                                                                      true)
                                                                                     &&
                                                                                     true))
                                                                                 {
                                                                                   return (x - 3);
                                                                                 }
                                                                                 else
                                                                                 {
                                                                                   args = [];
                                                                                   exhaust();
                                                                                 };
                                                                               });
                                                                     })())((5 + 5)))).
                                  toEqual(4);
                                }));
                            it("(λx = x + 1) .: (λx = x * 2) .: (λx = x - 3) 4 == 3",
                               (function()
                                {
                                  expect(_comp_col((function()
                                                    {
                                                      return (function(__a__)
                                                              {
                                                                var x;
                                                                x = null;
                                                                if((((function()
                                                                      {
                                                                        if((typeof(__a__)
                                                                            !=
                                                                            "undefined"))
                                                                        {
                                                                          x = __a__;
                                                                          return true;
                                                                        }
                                                                        else
                                                                        {
                                                                          return false;
                                                                        };
                                                                      })()
                                                                     &&
                                                                     true)
                                                                    &&
                                                                    true))
                                                                {
                                                                  return (x + 1);
                                                                }
                                                                else
                                                                {
                                                                  args = [];
                                                                  exhaust();
                                                                };
                                                              });
                                                    })())(_comp_col((function()
                                                                     {
                                                                       return (function(__a__)
                                                                               {
                                                                                 var x;
                                                                                 x = null;
                                                                                 if((((function()
                                                                                       {
                                                                                         if((typeof(__a__)
                                                                                             !=
                                                                                             "undefined"))
                                                                                         {
                                                                                           x = __a__;
                                                                                           return true;
                                                                                         }
                                                                                         else
                                                                                         {
                                                                                           return false;
                                                                                         };
                                                                                       })()
                                                                                      &&
                                                                                      true)
                                                                                     &&
                                                                                     true))
                                                                                 {
                                                                                   return (x * 2);
                                                                                 }
                                                                                 else
                                                                                 {
                                                                                   args = [];
                                                                                   exhaust();
                                                                                 };
                                                                               });
                                                                     })())((function()
                                                                            {
                                                                              return (function(__a__)
                                                                                      {
                                                                                        var x;
                                                                                        x = null;
                                                                                        if((((function()
                                                                                              {
                                                                                                if((typeof(__a__)
                                                                                                    !=
                                                                                                    "undefined"))
                                                                                                {
                                                                                                  x = __a__;
                                                                                                  return true;
                                                                                                }
                                                                                                else
                                                                                                {
                                                                                                  return false;
                                                                                                };
                                                                                              })()
                                                                                             &&
                                                                                             true)
                                                                                            &&
                                                                                            true))
                                                                                        {
                                                                                          return (x
                                                                                                  -
                                                                                                  3);
                                                                                        }
                                                                                        else
                                                                                        {
                                                                                          args = [];
                                                                                          exhaust();
                                                                                        };
                                                                                      });
                                                                            })()))(4)).
                                  toEqual(3);
                                }));
                            it("id [ 1, 2, 3 ] == [ 1, 2, 3 ]",
                               (function()
                                {
                                  expect(id([1, 2, 3])).toEqual([1, 2, 3]);
                                }));
                            it("flip (λx y = x - y) 3 5 == 2",
                               (function()
                                {
                                  expect(flip((function()
                                               {
                                                 return (function(__b__)
                                                         {
                                                           return (function(__a__)
                                                                   {
                                                                     var x;
                                                                     x = null;
                                                                     var y;
                                                                     y = null;
                                                                     if((((function()
                                                                           {
                                                                             if((typeof(__b__)
                                                                                 !=
                                                                                 "undefined"))
                                                                             {
                                                                               x = __b__;
                                                                               return true;
                                                                             }
                                                                             else
                                                                             {
                                                                               return false;
                                                                             };
                                                                           })()
                                                                          &&
                                                                          ((function()
                                                                            {
                                                                              if((typeof(__a__)
                                                                                  !=
                                                                                  "undefined"))
                                                                              {
                                                                                y = __a__;
                                                                                return true;
                                                                              }
                                                                              else
                                                                              {
                                                                                return false;
                                                                              };
                                                                            })()
                                                                           &&
                                                                           true))
                                                                         &&
                                                                         true))
                                                                     {
                                                                       return (x - y);
                                                                     }
                                                                     else
                                                                     {
                                                                       args = [];
                                                                       exhaust();
                                                                     };
                                                                   });
                                                         });
                                               })())(3)(5)).
                                  toEqual(2);
                                }));
                            it("option 3 some: (2) == 2",
                               (function()
                                {
                                  expect(option(3)((function()
                                                    {
                                                      var jmId_13;
                                                      jmId_13 = {};
                                                      jmId_13["some"] = 2;
                                                      return jmId_13;
                                                    })())).
                                  toEqual(2);
                                }));
                            describe("list",
                                     (function()
                                      {
                                        var _col_col;
                                        _col_col = prelude.list["_col_col"];
                                        var empty_;
                                        empty_ = prelude.list["empty_"];
                                        var head;
                                        head = prelude.list["head"];
                                        var tail;
                                        tail = prelude.list["tail"];
                                        var last;
                                        last = prelude.list["last"];
                                        var take;
                                        take = prelude.list["take"];
                                        var drop;
                                        drop = prelude.list["drop"];
                                        var take_while;
                                        take_while = prelude.list["take_while"];
                                        var drop_while;
                                        drop_while = prelude.list["drop_while"];
                                        var length;
                                        length = prelude.list["length"];
                                        var init;
                                        init = prelude.list["init"];
                                        var _plus_plus;
                                        _plus_plus = prelude.list["_plus_plus"];
                                        var filter;
                                        filter = prelude.list["filter"];
                                        var map;
                                        map = prelude.list["map"];
                                        var reverse;
                                        reverse = prelude.list["reverse"];
                                        var foldl;
                                        foldl = prelude.list["foldl"];
                                        var foldl1;
                                        foldl1 = prelude.list["foldl1"];
                                        var foldr;
                                        foldr = prelude.list["foldr"];
                                        var foldr1;
                                        foldr1 = prelude.list["foldr1"];
                                        var all_;
                                        all_ = prelude.list["all_"];
                                        var any_;
                                        any_ = prelude.list["any_"];
                                        var sum;
                                        sum = prelude.list["sum"];
                                        var product;
                                        product = prelude.list["product"];
                                        var concat;
                                        concat = prelude.list["concat"];
                                        var concat_map;
                                        concat_map = prelude.list["concat_map"];
                                        var maximum;
                                        maximum = prelude.list["maximum"];
                                        var minimum;
                                        minimum = prelude.list["minimum"];
                                        var jmId_14;
                                        jmId_14 = new (function()
                                                       {
                                                         it("{ head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == 1 :: 2 :: 3 :: nil:",
                                                            (function()
                                                             {
                                                               expect({ 'head': 1,
                                                                        'tail': { 'head': 2,
                                                                                  'tail': { 'head': 3,
                                                                                            'tail': { 'nil': true
                                                                                                    }
                                                                                          }
                                                                                }
                                                                      }).
                                                               toEqual(_col_col(1)(_col_col(2)(_col_col(3)((function()
                                                                                                            {
                                                                                                              var jmId_15;
                                                                                                              jmId_15 = {};
                                                                                                              jmId_15["nil"] = true;
                                                                                                              return jmId_15;
                                                                                                            })()))));
                                                             }));
                                                         it("1 :: 2 :: nil: == 1 :: 2 :: nil:",
                                                            (function()
                                                             {
                                                               expect(_col_col(1)(_col_col(2)((function()
                                                                                               {
                                                                                                 var jmId_16;
                                                                                                 jmId_16 = {};
                                                                                                 jmId_16["nil"] = true;
                                                                                                 return jmId_16;
                                                                                               })()))).
                                                               toEqual(_col_col(1)(_col_col(2)((function()
                                                                                                {
                                                                                                  var jmId_17;
                                                                                                  jmId_17 = {};
                                                                                                  jmId_17["nil"] = true;
                                                                                                  return jmId_17;
                                                                                                })())));
                                                             }));
                                                         it("nil: :: nil: == { head = nil:, tail = nil: }",
                                                            (function()
                                                             {
                                                               expect(_col_col((function()
                                                                                {
                                                                                  var jmId_18;
                                                                                  jmId_18 = {};
                                                                                  jmId_18["nil"] = true;
                                                                                  return jmId_18;
                                                                                })())((function()
                                                                                       {
                                                                                         var jmId_19;
                                                                                         jmId_19 = {};
                                                                                         jmId_19["nil"] = true;
                                                                                         return jmId_19;
                                                                                       })())).
                                                               toEqual({ 'head': (function()
                                                                                  {
                                                                                    var jmId_20;
                                                                                    jmId_20 = {};
                                                                                    jmId_20["nil"] = true;
                                                                                    return jmId_20;
                                                                                  })(),
                                                                         'tail': (function()
                                                                                  {
                                                                                    var jmId_21;
                                                                                    jmId_21 = {};
                                                                                    jmId_21["nil"] = true;
                                                                                    return jmId_21;
                                                                                  })()
                                                                       });
                                                             }));
                                                         it("3 :: 4 :: nil: == { head = 3, tail = { head = 4, tail = nil: } }",
                                                            (function()
                                                             {
                                                               expect(_col_col(3)(_col_col(4)((function()
                                                                                               {
                                                                                                 var jmId_22;
                                                                                                 jmId_22 = {};
                                                                                                 jmId_22["nil"] = true;
                                                                                                 return jmId_22;
                                                                                               })()))).
                                                               toEqual({ 'head': 3,
                                                                         'tail': { 'head': 4,
                                                                                   'tail': (function()
                                                                                            {
                                                                                              var jmId_23;
                                                                                              jmId_23 = {};
                                                                                              jmId_23["nil"] = true;
                                                                                              return jmId_23;
                                                                                            })()
                                                                                 }
                                                                       });
                                                             }));
                                                         it("not empty_ 1 :: nil:",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return not(empty_(_col_col(1)((function()
                                                                                                          {
                                                                                                            var jmId_25;
                                                                                                            jmId_25 = {};
                                                                                                            jmId_25["nil"] = true;
                                                                                                            return jmId_25;
                                                                                                          })())));
                                                                         }
                                                                         catch(jmId_24)
                                                                         {
                                                                           return { 'error': jmId_24
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("empty_ nil:",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return empty_((function()
                                                                                          {
                                                                                            var jmId_27;
                                                                                            jmId_27 = {};
                                                                                            jmId_27["nil"] = true;
                                                                                            return jmId_27;
                                                                                          })());
                                                                         }
                                                                         catch(jmId_26)
                                                                         {
                                                                           return { 'error': jmId_26
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("head { head = 1, tail = { head = 2, tail = { nil = true } } } == 1",
                                                            (function()
                                                             {
                                                               expect(head({ 'head': 1,
                                                                             'tail': { 'head': 2,
                                                                                       'tail': { 'nil': true
                                                                                               }
                                                                                     }
                                                                           })).
                                                               toEqual(1);
                                                             }));
                                                         it("tail { head = 1, tail = { head = 2, tail = { nil = true } } } == 2 :: nil:",
                                                            (function()
                                                             {
                                                               expect(tail({ 'head': 1,
                                                                             'tail': { 'head': 2,
                                                                                       'tail': { 'nil': true
                                                                                               }
                                                                                     }
                                                                           })).
                                                               toEqual(_col_col(2)((function()
                                                                                    {
                                                                                      var jmId_28;
                                                                                      jmId_28 = {};
                                                                                      jmId_28["nil"] = true;
                                                                                      return jmId_28;
                                                                                    })()));
                                                             }));
                                                         it("last { head = 1, tail = { head = 2, tail = { nil = true } } } == 2",
                                                            (function()
                                                             {
                                                               expect(last({ 'head': 1,
                                                                             'tail': { 'head': 2,
                                                                                       'tail': { 'nil': true
                                                                                               }
                                                                                     }
                                                                           })).
                                                               toEqual(2);
                                                             }));
                                                         it("err (λ_ = head nil:) == \"Head called on empty list\"",
                                                            (function()
                                                             {
                                                               expect(err((function()
                                                                           {
                                                                             return (function(__a__)
                                                                                     {
                                                                                       if(((true
                                                                                            &&
                                                                                            true)
                                                                                           &&
                                                                                           true))
                                                                                       {
                                                                                         return head((function()
                                                                                                      {
                                                                                                        var jmId_29;
                                                                                                        jmId_29 = {};
                                                                                                        jmId_29["nil"] = true;
                                                                                                        return jmId_29;
                                                                                                      })());
                                                                                       }
                                                                                       else
                                                                                       {
                                                                                         args = [];
                                                                                         exhaust();
                                                                                       };
                                                                                     });
                                                                           })())).
                                                               toEqual("Head called on empty list");
                                                             }));
                                                         it("err (λ_ = tail nil:) == \"Tail called on empty list\"",
                                                            (function()
                                                             {
                                                               expect(err((function()
                                                                           {
                                                                             return (function(__a__)
                                                                                     {
                                                                                       if(((true
                                                                                            &&
                                                                                            true)
                                                                                           &&
                                                                                           true))
                                                                                       {
                                                                                         return tail((function()
                                                                                                      {
                                                                                                        var jmId_30;
                                                                                                        jmId_30 = {};
                                                                                                        jmId_30["nil"] = true;
                                                                                                        return jmId_30;
                                                                                                      })());
                                                                                       }
                                                                                       else
                                                                                       {
                                                                                         args = [];
                                                                                         exhaust();
                                                                                       };
                                                                                     });
                                                                           })())).
                                                               toEqual("Tail called on empty list");
                                                             }));
                                                         it("err (λ_ = last nil:) == \"Last called on empty list\"",
                                                            (function()
                                                             {
                                                               expect(err((function()
                                                                           {
                                                                             return (function(__a__)
                                                                                     {
                                                                                       if(((true
                                                                                            &&
                                                                                            true)
                                                                                           &&
                                                                                           true))
                                                                                       {
                                                                                         return last((function()
                                                                                                      {
                                                                                                        var jmId_31;
                                                                                                        jmId_31 = {};
                                                                                                        jmId_31["nil"] = true;
                                                                                                        return jmId_31;
                                                                                                      })());
                                                                                       }
                                                                                       else
                                                                                       {
                                                                                         args = [];
                                                                                         exhaust();
                                                                                       };
                                                                                     });
                                                                           })())).
                                                               toEqual("Last called on empty list");
                                                             }));
                                                         it("take 2 { head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == { head = 1, tail = { head = 2, tail = { nil = true } } }",
                                                            (function()
                                                             {
                                                               expect(take(2)({ 'head': 1,
                                                                                'tail': { 'head': 2,
                                                                                          'tail': { 'head': 3,
                                                                                                    'tail': { 'nil': true
                                                                                                            }
                                                                                                  }
                                                                                        }
                                                                              })).
                                                               toEqual({ 'head': 1,
                                                                         'tail': { 'head': 2,
                                                                                   'tail': { 'nil': true
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("drop 2 { head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == { head = 3, tail = { nil = true } }",
                                                            (function()
                                                             {
                                                               expect(drop(2)({ 'head': 1,
                                                                                'tail': { 'head': 2,
                                                                                          'tail': { 'head': 3,
                                                                                                    'tail': { 'nil': true
                                                                                                            }
                                                                                                  }
                                                                                        }
                                                                              })).
                                                               toEqual({ 'head': 3,
                                                                         'tail': { 'nil': true
                                                                                 }
                                                                       });
                                                             }));
                                                         it("take_while (λx = x < 0) { nil = true } == { nil = true }",
                                                            (function()
                                                             {
                                                               expect(take_while((function()
                                                                                  {
                                                                                    return (function(__a__)
                                                                                            {
                                                                                              var x;
                                                                                              x = null;
                                                                                              if((((function()
                                                                                                    {
                                                                                                      if((typeof(__a__)
                                                                                                          !=
                                                                                                          "undefined"))
                                                                                                      {
                                                                                                        x = __a__;
                                                                                                        return true;
                                                                                                      }
                                                                                                      else
                                                                                                      {
                                                                                                        return false;
                                                                                                      };
                                                                                                    })()
                                                                                                   &&
                                                                                                   true)
                                                                                                  &&
                                                                                                  true))
                                                                                              {
                                                                                                return _less(x)(0);
                                                                                              }
                                                                                              else
                                                                                              {
                                                                                                args = [];
                                                                                                exhaust();
                                                                                              };
                                                                                            });
                                                                                  })())({ 'nil': true
                                                                                        })).
                                                               toEqual({ 'nil': true
                                                                       });
                                                             }));
                                                         it("take_while (λx = x > 0) { head = 2, tail = { head = 1, tail = { head = 0, tail = { nil = true } } } } == { head = 2, tail = { head = 1, tail = { nil = true } } }",
                                                            (function()
                                                             {
                                                               expect(take_while((function()
                                                                                  {
                                                                                    return (function(__a__)
                                                                                            {
                                                                                              var x;
                                                                                              x = null;
                                                                                              if((((function()
                                                                                                    {
                                                                                                      if((typeof(__a__)
                                                                                                          !=
                                                                                                          "undefined"))
                                                                                                      {
                                                                                                        x = __a__;
                                                                                                        return true;
                                                                                                      }
                                                                                                      else
                                                                                                      {
                                                                                                        return false;
                                                                                                      };
                                                                                                    })()
                                                                                                   &&
                                                                                                   true)
                                                                                                  &&
                                                                                                  true))
                                                                                              {
                                                                                                return _grea(x)(0);
                                                                                              }
                                                                                              else
                                                                                              {
                                                                                                args = [];
                                                                                                exhaust();
                                                                                              };
                                                                                            });
                                                                                  })())({ 'head': 2,
                                                                                          'tail': { 'head': 1,
                                                                                                    'tail': { 'head': 0,
                                                                                                              'tail': { 'nil': true
                                                                                                                      }
                                                                                                            }
                                                                                                  }
                                                                                        })).
                                                               toEqual({ 'head': 2,
                                                                         'tail': { 'head': 1,
                                                                                   'tail': { 'nil': true
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("drop_while (λx = x < 0) nil: == nil:",
                                                            (function()
                                                             {
                                                               expect(drop_while((function()
                                                                                  {
                                                                                    return (function(__a__)
                                                                                            {
                                                                                              var x;
                                                                                              x = null;
                                                                                              if((((function()
                                                                                                    {
                                                                                                      if((typeof(__a__)
                                                                                                          !=
                                                                                                          "undefined"))
                                                                                                      {
                                                                                                        x = __a__;
                                                                                                        return true;
                                                                                                      }
                                                                                                      else
                                                                                                      {
                                                                                                        return false;
                                                                                                      };
                                                                                                    })()
                                                                                                   &&
                                                                                                   true)
                                                                                                  &&
                                                                                                  true))
                                                                                              {
                                                                                                return _less(x)(0);
                                                                                              }
                                                                                              else
                                                                                              {
                                                                                                args = [];
                                                                                                exhaust();
                                                                                              };
                                                                                            });
                                                                                  })())((function()
                                                                                         {
                                                                                           var jmId_32;
                                                                                           jmId_32 = {};
                                                                                           jmId_32["nil"] = true;
                                                                                           return jmId_32;
                                                                                         })())).
                                                               toEqual((function()
                                                                        {
                                                                          var jmId_33;
                                                                          jmId_33 = {};
                                                                          jmId_33["nil"] = true;
                                                                          return jmId_33;
                                                                        })());
                                                             }));
                                                         it("drop_while (λx = x > 0) { head = 2, tail = { head = 1, tail = { head = 0, tail = { nil = true } } } } == { head = 0, tail = { nil = true } }",
                                                            (function()
                                                             {
                                                               expect(drop_while((function()
                                                                                  {
                                                                                    return (function(__a__)
                                                                                            {
                                                                                              var x;
                                                                                              x = null;
                                                                                              if((((function()
                                                                                                    {
                                                                                                      if((typeof(__a__)
                                                                                                          !=
                                                                                                          "undefined"))
                                                                                                      {
                                                                                                        x = __a__;
                                                                                                        return true;
                                                                                                      }
                                                                                                      else
                                                                                                      {
                                                                                                        return false;
                                                                                                      };
                                                                                                    })()
                                                                                                   &&
                                                                                                   true)
                                                                                                  &&
                                                                                                  true))
                                                                                              {
                                                                                                return _grea(x)(0);
                                                                                              }
                                                                                              else
                                                                                              {
                                                                                                args = [];
                                                                                                exhaust();
                                                                                              };
                                                                                            });
                                                                                  })())({ 'head': 2,
                                                                                          'tail': { 'head': 1,
                                                                                                    'tail': { 'head': 0,
                                                                                                              'tail': { 'nil': true
                                                                                                                      }
                                                                                                            }
                                                                                                  }
                                                                                        })).
                                                               toEqual({ 'head': 0,
                                                                         'tail': { 'nil': true
                                                                                 }
                                                                       });
                                                             }));
                                                         it("length { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } } == 4",
                                                            (function()
                                                             {
                                                               expect(length({ 'head': 1,
                                                                               'tail': { 'head': 2,
                                                                                         'tail': { 'head': 3,
                                                                                                   'tail': { 'head': 4,
                                                                                                             'tail': { 'nil': true
                                                                                                                     }
                                                                                                           }
                                                                                                 }
                                                                                       }
                                                                             })).
                                                               toEqual(4);
                                                             }));
                                                         it("length init 30 0 == 30",
                                                            (function()
                                                             {
                                                               expect(length(init(30)(0))).
                                                               toEqual(30);
                                                             }));
                                                         it("tail init 3 0 == init 2 0",
                                                            (function()
                                                             {
                                                               expect(tail(init(3)(0))).
                                                               toEqual(init(2)(0));
                                                             }));
                                                         it("nil: ++ 1 :: 2 :: nil: == 1 :: 2 :: nil:",
                                                            (function()
                                                             {
                                                               expect(_plus_plus((function()
                                                                                  {
                                                                                    var jmId_34;
                                                                                    jmId_34 = {};
                                                                                    jmId_34["nil"] = true;
                                                                                    return jmId_34;
                                                                                  })())(_col_col(1)(_col_col(2)((function()
                                                                                                                 {
                                                                                                                   var jmId_35;
                                                                                                                   jmId_35 = {};
                                                                                                                   jmId_35["nil"] = true;
                                                                                                                   return jmId_35;
                                                                                                                 })())))).
                                                               toEqual(_col_col(1)(_col_col(2)((function()
                                                                                                {
                                                                                                  var jmId_36;
                                                                                                  jmId_36 = {};
                                                                                                  jmId_36["nil"] = true;
                                                                                                  return jmId_36;
                                                                                                })())));
                                                             }));
                                                         it("{ head = 1, tail = { head = 2, tail = { nil = true } } } ++ { head = 3, tail = { head = 4, tail = { nil = true } } } == { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } }",
                                                            (function()
                                                             {
                                                               expect(_plus_plus({ 'head': 1,
                                                                                   'tail': { 'head': 2,
                                                                                             'tail': { 'nil': true
                                                                                                     }
                                                                                           }
                                                                                 })({ 'head': 3,
                                                                                      'tail': { 'head': 4,
                                                                                                'tail': { 'nil': true
                                                                                                        }
                                                                                              }
                                                                                    })).
                                                               toEqual({ 'head': 1,
                                                                         'tail': { 'head': 2,
                                                                                   'tail': { 'head': 3,
                                                                                             'tail': { 'head': 4,
                                                                                                       'tail': { 'nil': true
                                                                                                               }
                                                                                                     }
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("filter (λx = x > 2) { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } } == { head = 3, tail = { head = 4, tail = { nil = true } } }",
                                                            (function()
                                                             {
                                                               expect(filter((function()
                                                                              {
                                                                                return (function(__a__)
                                                                                        {
                                                                                          var x;
                                                                                          x = null;
                                                                                          if((((function()
                                                                                                {
                                                                                                  if((typeof(__a__)
                                                                                                      !=
                                                                                                      "undefined"))
                                                                                                  {
                                                                                                    x = __a__;
                                                                                                    return true;
                                                                                                  }
                                                                                                  else
                                                                                                  {
                                                                                                    return false;
                                                                                                  };
                                                                                                })()
                                                                                               &&
                                                                                               true)
                                                                                              &&
                                                                                              true))
                                                                                          {
                                                                                            return _grea(x)(2);
                                                                                          }
                                                                                          else
                                                                                          {
                                                                                            args = [];
                                                                                            exhaust();
                                                                                          };
                                                                                        });
                                                                              })())({ 'head': 1,
                                                                                      'tail': { 'head': 2,
                                                                                                'tail': { 'head': 3,
                                                                                                          'tail': { 'head': 4,
                                                                                                                    'tail': { 'nil': true
                                                                                                                            }
                                                                                                                  }
                                                                                                        }
                                                                                              }
                                                                                    })).
                                                               toEqual({ 'head': 3,
                                                                         'tail': { 'head': 4,
                                                                                   'tail': { 'nil': true
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("map (λx = x + 1) { head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } }",
                                                            (function()
                                                             {
                                                               expect(map((function()
                                                                           {
                                                                             return (function(__a__)
                                                                                     {
                                                                                       var x;
                                                                                       x = null;
                                                                                       if((((function()
                                                                                             {
                                                                                               if((typeof(__a__)
                                                                                                   !=
                                                                                                   "undefined"))
                                                                                               {
                                                                                                 x = __a__;
                                                                                                 return true;
                                                                                               }
                                                                                               else
                                                                                               {
                                                                                                 return false;
                                                                                               };
                                                                                             })()
                                                                                            &&
                                                                                            true)
                                                                                           &&
                                                                                           true))
                                                                                       {
                                                                                         return (x
                                                                                                 +
                                                                                                 1);
                                                                                       }
                                                                                       else
                                                                                       {
                                                                                         args = [];
                                                                                         exhaust();
                                                                                       };
                                                                                     });
                                                                           })())({ 'head': 1,
                                                                                   'tail': { 'head': 2,
                                                                                             'tail': { 'head': 3,
                                                                                                       'tail': { 'nil': true
                                                                                                               }
                                                                                                     }
                                                                                           }
                                                                                 })).
                                                               toEqual({ 'head': 2,
                                                                         'tail': { 'head': 3,
                                                                                   'tail': { 'head': 4,
                                                                                             'tail': { 'nil': true
                                                                                                     }
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("reverse { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } } == { head = 4, tail = { head = 3, tail = { head = 2, tail = { head = 1, tail = { nil = true } } } } }",
                                                            (function()
                                                             {
                                                               expect(reverse({ 'head': 1,
                                                                                'tail': { 'head': 2,
                                                                                          'tail': { 'head': 3,
                                                                                                    'tail': { 'head': 4,
                                                                                                              'tail': { 'nil': true
                                                                                                                      }
                                                                                                            }
                                                                                                  }
                                                                                        }
                                                                              })).
                                                               toEqual({ 'head': 4,
                                                                         'tail': { 'head': 3,
                                                                                   'tail': { 'head': 2,
                                                                                             'tail': { 'head': 1,
                                                                                                       'tail': { 'nil': true
                                                                                                               }
                                                                                                     }
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("foldl (λx y = x + y) 0 { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } } == 10",
                                                            (function()
                                                             {
                                                               expect(foldl((function()
                                                                             {
                                                                               return (function(__b__)
                                                                                       {
                                                                                         return (function(__a__)
                                                                                                 {
                                                                                                   var x;
                                                                                                   x = null;
                                                                                                   var y;
                                                                                                   y = null;
                                                                                                   if((((function()
                                                                                                         {
                                                                                                           if((typeof(__b__)
                                                                                                               !=
                                                                                                               "undefined"))
                                                                                                           {
                                                                                                             x = __b__;
                                                                                                             return true;
                                                                                                           }
                                                                                                           else
                                                                                                           {
                                                                                                             return false;
                                                                                                           };
                                                                                                         })()
                                                                                                        &&
                                                                                                        ((function()
                                                                                                          {
                                                                                                            if((typeof(__a__)
                                                                                                                !=
                                                                                                                "undefined"))
                                                                                                            {
                                                                                                              y = __a__;
                                                                                                              return true;
                                                                                                            }
                                                                                                            else
                                                                                                            {
                                                                                                              return false;
                                                                                                            };
                                                                                                          })()
                                                                                                         &&
                                                                                                         true))
                                                                                                       &&
                                                                                                       true))
                                                                                                   {
                                                                                                     return (x
                                                                                                             +
                                                                                                             y);
                                                                                                   }
                                                                                                   else
                                                                                                   {
                                                                                                     args = [];
                                                                                                     exhaust();
                                                                                                   };
                                                                                                 });
                                                                                       });
                                                                             })())(0)({ 'head': 1,
                                                                                        'tail': { 'head': 2,
                                                                                                  'tail': { 'head': 3,
                                                                                                            'tail': { 'head': 4,
                                                                                                                      'tail': { 'nil': true
                                                                                                                              }
                                                                                                                    }
                                                                                                          }
                                                                                                }
                                                                                      })).
                                                               toEqual(10);
                                                             }));
                                                         it("foldr (λx y = x + y) 0 { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } } == 10",
                                                            (function()
                                                             {
                                                               expect(foldr((function()
                                                                             {
                                                                               return (function(__b__)
                                                                                       {
                                                                                         return (function(__a__)
                                                                                                 {
                                                                                                   var x;
                                                                                                   x = null;
                                                                                                   var y;
                                                                                                   y = null;
                                                                                                   if((((function()
                                                                                                         {
                                                                                                           if((typeof(__b__)
                                                                                                               !=
                                                                                                               "undefined"))
                                                                                                           {
                                                                                                             x = __b__;
                                                                                                             return true;
                                                                                                           }
                                                                                                           else
                                                                                                           {
                                                                                                             return false;
                                                                                                           };
                                                                                                         })()
                                                                                                        &&
                                                                                                        ((function()
                                                                                                          {
                                                                                                            if((typeof(__a__)
                                                                                                                !=
                                                                                                                "undefined"))
                                                                                                            {
                                                                                                              y = __a__;
                                                                                                              return true;
                                                                                                            }
                                                                                                            else
                                                                                                            {
                                                                                                              return false;
                                                                                                            };
                                                                                                          })()
                                                                                                         &&
                                                                                                         true))
                                                                                                       &&
                                                                                                       true))
                                                                                                   {
                                                                                                     return (x
                                                                                                             +
                                                                                                             y);
                                                                                                   }
                                                                                                   else
                                                                                                   {
                                                                                                     args = [];
                                                                                                     exhaust();
                                                                                                   };
                                                                                                 });
                                                                                       });
                                                                             })())(0)({ 'head': 1,
                                                                                        'tail': { 'head': 2,
                                                                                                  'tail': { 'head': 3,
                                                                                                            'tail': { 'head': 4,
                                                                                                                      'tail': { 'nil': true
                                                                                                                              }
                                                                                                                    }
                                                                                                          }
                                                                                                }
                                                                                      })).
                                                               toEqual(10);
                                                             }));
                                                         it("all_ id { head = true, tail = { head = true, tail = { nil = true } } }",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return all_(id)({ 'head': true,
                                                                                             'tail': { 'head': true,
                                                                                                       'tail': { 'nil': true
                                                                                                               }
                                                                                                     }
                                                                                           });
                                                                         }
                                                                         catch(jmId_37)
                                                                         {
                                                                           return { 'error': jmId_37
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("not all_ id { head = true, tail = { head = false, tail = { nil = true } } }",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return not(all_(id)({ 'head': true,
                                                                                                 'tail': { 'head': false,
                                                                                                           'tail': { 'nil': true
                                                                                                                   }
                                                                                                         }
                                                                                               }));
                                                                         }
                                                                         catch(jmId_38)
                                                                         {
                                                                           return { 'error': jmId_38
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("any_ id true :: false :: nil:",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return any_(id)(_col_col(true)(_col_col(false)((function()
                                                                                                                           {
                                                                                                                             var jmId_40;
                                                                                                                             jmId_40 = {};
                                                                                                                             jmId_40["nil"] = true;
                                                                                                                             return jmId_40;
                                                                                                                           })())));
                                                                         }
                                                                         catch(jmId_39)
                                                                         {
                                                                           return { 'error': jmId_39
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("not any_ id false :: false :: nil:",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return not(any_(id)(_col_col(false)(_col_col(false)((function()
                                                                                                                                {
                                                                                                                                  var jmId_42;
                                                                                                                                  jmId_42 = {};
                                                                                                                                  jmId_42["nil"] = true;
                                                                                                                                  return jmId_42;
                                                                                                                                })()))));
                                                                         }
                                                                         catch(jmId_41)
                                                                         {
                                                                           return { 'error': jmId_41
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("sum 1 :: 2 :: 3 :: nil: == 6",
                                                            (function()
                                                             {
                                                               expect(sum(_col_col(1)(_col_col(2)(_col_col(3)((function()
                                                                                                               {
                                                                                                                 var jmId_43;
                                                                                                                 jmId_43 = {};
                                                                                                                 jmId_43["nil"] = true;
                                                                                                                 return jmId_43;
                                                                                                               })()))))).
                                                               toEqual(6);
                                                             }));
                                                         it("product 1 :: 2 :: 4 :: nil: == 8",
                                                            (function()
                                                             {
                                                               expect(product(_col_col(1)(_col_col(2)(_col_col(4)((function()
                                                                                                                   {
                                                                                                                     var jmId_44;
                                                                                                                     jmId_44 = {};
                                                                                                                     jmId_44["nil"] = true;
                                                                                                                     return jmId_44;
                                                                                                                   })()))))).
                                                               toEqual(8);
                                                             }));
                                                         it("let x  = 1 :: 2 :: nil:\n| y  = 3 :: 4 :: nil: in (concat { head = x, tail = { head = y, tail = { nil = true } } } == { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } })",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return (function()
                                                                                   {
                                                                                     var x;
                                                                                     x = (function()
                                                                                          {
                                                                                            if((true
                                                                                                &&
                                                                                                true))
                                                                                            {
                                                                                              return _col_col(1)(_col_col(2)((function()
                                                                                                                              {
                                                                                                                                var jmId_46;
                                                                                                                                jmId_46 = {};
                                                                                                                                jmId_46["nil"] = true;
                                                                                                                                return jmId_46;
                                                                                                                              })()));
                                                                                            }
                                                                                            else
                                                                                            {
                                                                                              args = [];
                                                                                              exhaust();
                                                                                            };
                                                                                          })();
                                                                                     var y;
                                                                                     y = (function()
                                                                                          {
                                                                                            if((true
                                                                                                &&
                                                                                                true))
                                                                                            {
                                                                                              return _col_col(3)(_col_col(4)((function()
                                                                                                                              {
                                                                                                                                var jmId_47;
                                                                                                                                jmId_47 = {};
                                                                                                                                jmId_47["nil"] = true;
                                                                                                                                return jmId_47;
                                                                                                                              })()));
                                                                                            }
                                                                                            else
                                                                                            {
                                                                                              args = [];
                                                                                              exhaust();
                                                                                            };
                                                                                          })();
                                                                                     return _eq_eq(concat({ 'head': x,
                                                                                                            'tail': { 'head': y,
                                                                                                                      'tail': { 'nil': true
                                                                                                                              }
                                                                                                                    }
                                                                                                          }))({ 'head': 1,
                                                                                                                'tail': { 'head': 2,
                                                                                                                          'tail': { 'head': 3,
                                                                                                                                    'tail': { 'head': 4,
                                                                                                                                              'tail': { 'nil': true
                                                                                                                                                      }
                                                                                                                                            }
                                                                                                                                  }
                                                                                                                        }
                                                                                                              });
                                                                                   })();
                                                                         }
                                                                         catch(jmId_45)
                                                                         {
                                                                           return { 'error': jmId_45
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("concat_map (λx = { head = x, tail = { head = x + 1, tail = { nil = true } } }) { head = 1, tail = { head = 2, tail = { nil = true } } } == { head = 1, tail = { head = 2, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } }",
                                                            (function()
                                                             {
                                                               expect(concat_map((function()
                                                                                  {
                                                                                    return (function(__a__)
                                                                                            {
                                                                                              var x;
                                                                                              x = null;
                                                                                              if((((function()
                                                                                                    {
                                                                                                      if((typeof(__a__)
                                                                                                          !=
                                                                                                          "undefined"))
                                                                                                      {
                                                                                                        x = __a__;
                                                                                                        return true;
                                                                                                      }
                                                                                                      else
                                                                                                      {
                                                                                                        return false;
                                                                                                      };
                                                                                                    })()
                                                                                                   &&
                                                                                                   true)
                                                                                                  &&
                                                                                                  true))
                                                                                              {
                                                                                                return { 'head': x,
                                                                                                         'tail': { 'head': (x
                                                                                                                            +
                                                                                                                            1),
                                                                                                                   'tail': { 'nil': true
                                                                                                                           }
                                                                                                                 }
                                                                                                       };
                                                                                              }
                                                                                              else
                                                                                              {
                                                                                                args = [];
                                                                                                exhaust();
                                                                                              };
                                                                                            });
                                                                                  })())({ 'head': 1,
                                                                                          'tail': { 'head': 2,
                                                                                                    'tail': { 'nil': true
                                                                                                            }
                                                                                                  }
                                                                                        })).
                                                               toEqual({ 'head': 1,
                                                                         'tail': { 'head': 2,
                                                                                   'tail': { 'head': 2,
                                                                                             'tail': { 'head': 3,
                                                                                                       'tail': { 'nil': true
                                                                                                               }
                                                                                                     }
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("minimum { head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == 1",
                                                            (function()
                                                             {
                                                               expect(minimum({ 'head': 1,
                                                                                'tail': { 'head': 2,
                                                                                          'tail': { 'head': 3,
                                                                                                    'tail': { 'nil': true
                                                                                                            }
                                                                                                  }
                                                                                        }
                                                                              })).
                                                               toEqual(1);
                                                             }));
                                                         it("maximum { head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == 3",
                                                            (function()
                                                             {
                                                               expect(maximum({ 'head': 1,
                                                                                'tail': { 'head': 2,
                                                                                          'tail': { 'head': 3,
                                                                                                    'tail': { 'nil': true
                                                                                                            }
                                                                                                  }
                                                                                        }
                                                                              })).
                                                               toEqual(3);
                                                             }));
                                                       })();
                                      }));
                            describe("sequence",
                                     (function()
                                      {
                                        var _col_col;
                                        _col_col = list["_col_col"];
                                        var empty_;
                                        empty_ = list["empty_"];
                                        var head;
                                        head = list["head"];
                                        var tail;
                                        tail = list["tail"];
                                        var last;
                                        last = list["last"];
                                        var take;
                                        take = list["take"];
                                        var drop;
                                        drop = list["drop"];
                                        var take_while;
                                        take_while = list["take_while"];
                                        var drop_while;
                                        drop_while = list["drop_while"];
                                        var length;
                                        length = list["length"];
                                        var init;
                                        init = list["init"];
                                        var _plus_plus;
                                        _plus_plus = list["_plus_plus"];
                                        var filter;
                                        filter = list["filter"];
                                        var map;
                                        map = list["map"];
                                        var reverse;
                                        reverse = list["reverse"];
                                        var foldl;
                                        foldl = list["foldl"];
                                        var foldl1;
                                        foldl1 = list["foldl1"];
                                        var foldr;
                                        foldr = list["foldr"];
                                        var foldr1;
                                        foldr1 = list["foldr1"];
                                        var all_;
                                        all_ = list["all_"];
                                        var any_;
                                        any_ = list["any_"];
                                        var sum;
                                        sum = list["sum"];
                                        var product;
                                        product = list["product"];
                                        var concat;
                                        concat = list["concat"];
                                        var concat_map;
                                        concat_map = list["concat_map"];
                                        var maximum;
                                        maximum = list["maximum"];
                                        var minimum;
                                        minimum = list["minimum"];
                                        var from_list;
                                        from_list = prelude.sequence["from_list"];
                                        var to_list;
                                        to_list = prelude.sequence["to_list"];
                                        var iterate;
                                        iterate = prelude.sequence["iterate"];
                                        var match;
                                        match = prelude.sequence["match"];
                                        var take;
                                        take = prelude.sequence["take"];
                                        var jmId_48;
                                        jmId_48 = new (function()
                                                       {
                                                         it("to_list from_list { head = 1, tail = { head = 2, tail = { nil = true } } } == { head = 1, tail = { head = 2, tail = { nil = true } } }",
                                                            (function()
                                                             {
                                                               expect(to_list(from_list({ 'head': 1,
                                                                                          'tail': { 'head': 2,
                                                                                                    'tail': { 'nil': true
                                                                                                            }
                                                                                                  }
                                                                                        }))).
                                                               toEqual({ 'head': 1,
                                                                         'tail': { 'head': 2,
                                                                                   'tail': { 'nil': true
                                                                                           }
                                                                                 }
                                                                       });
                                                             }));
                                                         it("let f  = to_list .: take 3 .: from_list in ({ head = 1, tail = { head = 2, tail = { head = 3, tail = { nil = true } } } } == f { head = 1, tail = { head = 2, tail = { head = 3, tail = { head = 4, tail = { nil = true } } } } })",
                                                            (function()
                                                             {
                                                               expect((function()
                                                                       {
                                                                         try
                                                                         {
                                                                           return (function()
                                                                                   {
                                                                                     var f;
                                                                                     f = (function()
                                                                                          {
                                                                                            if((true
                                                                                                &&
                                                                                                true))
                                                                                            {
                                                                                              return _comp_col(to_list)(_comp_col(take(3))(from_list));
                                                                                            }
                                                                                            else
                                                                                            {
                                                                                              args = [];
                                                                                              exhaust();
                                                                                            };
                                                                                          })();
                                                                                     return _eq_eq({ 'head': 1,
                                                                                                     'tail': { 'head': 2,
                                                                                                               'tail': { 'head': 3,
                                                                                                                         'tail': { 'nil': true
                                                                                                                                 }
                                                                                                                       }
                                                                                                             }
                                                                                                   })(f({ 'head': 1,
                                                                                                          'tail': { 'head': 2,
                                                                                                                    'tail': { 'head': 3,
                                                                                                                              'tail': { 'head': 4,
                                                                                                                                        'tail': { 'nil': true
                                                                                                                                                }
                                                                                                                                      }
                                                                                                                            }
                                                                                                                  }
                                                                                                        }));
                                                                                   })();
                                                                         }
                                                                         catch(jmId_49)
                                                                         {
                                                                           return { 'error': jmId_49
                                                                                  };
                                                                         };
                                                                       })()).
                                                               toEqual(true);
                                                             }));
                                                         it("500 == iterate (λx = x + 1) 0 |> take 500 |> to_list |> length",
                                                            (function()
                                                             {
                                                               expect(500).
                                                               toEqual(_or_grea(_or_grea(_or_grea(iterate((function()
                                                                                                           {
                                                                                                             return (function(__a__)
                                                                                                                     {
                                                                                                                       var x;
                                                                                                                       x = null;
                                                                                                                       if((((function()
                                                                                                                             {
                                                                                                                               if((typeof(__a__)
                                                                                                                                   !=
                                                                                                                                   "undefined"))
                                                                                                                               {
                                                                                                                                 x = __a__;
                                                                                                                                 return true;
                                                                                                                               }
                                                                                                                               else
                                                                                                                               {
                                                                                                                                 return false;
                                                                                                                               };
                                                                                                                             })()
                                                                                                                            &&
                                                                                                                            true)
                                                                                                                           &&
                                                                                                                           true))
                                                                                                                       {
                                                                                                                         return (x
                                                                                                                                 +
                                                                                                                                 1);
                                                                                                                       }
                                                                                                                       else
                                                                                                                       {
                                                                                                                         args = [];
                                                                                                                         exhaust();
                                                                                                                       };
                                                                                                                     });
                                                                                                           })())(0))(take(500)))(to_list))(length));
                                                             }));
                                                       })();
                                      }));
                            describe("map",
                                     (function()
                                      {
                                        var _perc;
                                        _perc = prelude.map["_perc"];
                                        var jmId_50;
                                        jmId_50 = new (function()
                                                       {
                                                         it("\"test\" % true == { key = \"test\", val = true }",
                                                            (function()
                                                             {
                                                               expect(_perc("test")(true)).
                                                               toEqual({ 'key': "test", 'val': true
                                                                       });
                                                             }));
                                                       })();
                                      }));
                          })();
          }));
var object_;
object_ = prelude["object_"];
var array_;
array_ = prelude["array_"];
var not;
not = prelude["not"];
var run;
run = prelude["run"];
var _grea_grea_eq;
_grea_grea_eq = prelude["_grea_grea_eq"];
var ret;
ret = prelude["ret"];
var _grea_grea;
_grea_grea = prelude["_grea_grea"];
var log;
log = prelude["log"];
var _and_and;
_and_and = prelude["_and_and"];
var _or_or;
_or_or = prelude["_or_or"];
var _star;
_star = prelude["_star"];
var _forw;
_forw = prelude["_forw"];
var _plus;
_plus = prelude["_plus"];
var _minu;
_minu = prelude["_minu"];
var _less_eq;
_less_eq = prelude["_less_eq"];
var _grea_eq;
_grea_eq = prelude["_grea_eq"];
var _less;
_less = prelude["_less"];
var _grea;
_grea = prelude["_grea"];
var _eq_eq;
_eq_eq = prelude["_eq_eq"];
var _bang_eq;
_bang_eq = prelude["_bang_eq"];
var fib;
fib = prelude["fib"];
var time;
time = prelude["time"];
var fast_fib;
fast_fib = prelude["fast_fib"];
var floor;
floor = prelude["floor"];
var _less_or;
_less_or = prelude["_less_or"];
var _or_grea;
_or_grea = prelude["_or_grea"];
var _less_col;
_less_col = prelude["_less_col"];
var _comp_col;
_comp_col = prelude["_comp_col"];
var id;
id = prelude["id"];
var flip;
flip = prelude["flip"];
var err;
err = prelude["err"];
var option;
option = prelude["option"];
var _col_col;
_col_col = prelude["_col_col"];
var empty_;
empty_ = prelude["empty_"];
var head;
head = prelude["head"];
var tail;
tail = prelude["tail"];
var last;
last = prelude["last"];
var take;
take = prelude["take"];
var drop;
drop = prelude["drop"];
var take_while;
take_while = prelude["take_while"];
var drop_while;
drop_while = prelude["drop_while"];
var length;
length = prelude["length"];
var init;
init = prelude["init"];
var _plus_plus;
_plus_plus = prelude["_plus_plus"];
var filter;
filter = prelude["filter"];
var map;
map = prelude["map"];
var reverse;
reverse = prelude["reverse"];
var foldl;
foldl = prelude["foldl"];
var foldl1;
foldl1 = prelude["foldl1"];
var foldr;
foldr = prelude["foldr"];
var foldr1;
foldr1 = prelude["foldr1"];
var all_;
all_ = prelude["all_"];
var any_;
any_ = prelude["any_"];
var sum;
sum = prelude["sum"];
var product;
product = prelude["product"];
var concat;
concat = prelude["concat"];
var concat_map;
concat_map = prelude["concat_map"];
var maximum;
maximum = prelude["maximum"];
var minimum;
minimum = prelude["minimum"];
var from_list;
from_list = prelude["from_list"];
var to_list;
to_list = prelude["to_list"];
var iterate;
iterate = prelude["iterate"];
var match;
match = prelude["match"];
var take;
take = prelude["take"];
var _perc;
_perc = prelude["_perc"];