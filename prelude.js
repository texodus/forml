var g = !0;
function h(q) {
  return q instanceof Array
}
function v() {
  throw"Pattern Match Exhausted";
}
var D = new function() {
  function q(a) {
    return function(b) {
      return function() {
        return function(c) {
          return a(b(c))
        }
      }()
    }
  }
  function y(a) {
    return function(b) {
      return a(b)
    }
  }
  function t(a) {
    return 0 === a ? 0 : 1 === a ? 1 : t(a - 1) + t(a - 2)
  }
  function k(a) {
    return function(b) {
      var c, f;
      c = a;
      f = b;
      if(g && r(c) && r(f)) {
        return function() {
          var a = g;
          return e(function() {
            for(var b in c) {
              a = a && f.hasOwnProperty(b) && k(c[b])(f[b])
            }
            b = Object.keys(c).length === Object.keys(f).length;
            return a && b
          })
        }()
      }
      c = a;
      f = b;
      if(g && h(c) && h(f)) {
        return function() {
          var a = g;
          return e(function() {
            for(var b in c) {
              a = a && k(c[b])(f[b])
            }
            return a && c.length == f.length
          })
        }()
      }
      c = a;
      f = b;
      return e(function() {
        return c === f
      })
    }
  }
  function w(a) {
    return function(b) {
      return e(function() {
        return a > b
      })
    }
  }
  function x(a) {
    return function() {
      return function() {
        return a
      }
    }()
  }
  function l(a) {
    return function(b) {
      return function() {
        return function() {
          return e(b(e(a)))
        }
      }()
    }
  }
  function e(a) {
    return a(null)
  }
  function z(a) {
    return e(function() {
      return!a
    })
  }
  function r(a) {
    return e(function() {
      return"object" === typeof a
    })
  }
  this.object_ = r;
  this.array_ = h;
  this.not = z;
  this.run = e;
  this._grea_grea_eq = l;
  this.ret = x;
  this._grea_grea = function(a) {
    return function(b) {
      return function() {
        a();
        return b()
      }
    }
  };
  this.log = function(a) {
    return function() {
      return console.log(a)
    }
  };
  this._and_and = function(a) {
    return function(b) {
      return e(function() {
        return a && b
      })
    }
  };
  this._or_or = function(a) {
    return function(b) {
      return e(function() {
        return a || b
      })
    }
  };
  this._star = function(a) {
    return function(b) {
      return e(function() {
        return a * b
      })
    }
  };
  this._forw = function(a) {
    return function(b) {
      return e(function() {
        return a / b
      })
    }
  };
  this._plus = function(a) {
    return function(b) {
      return e(function() {
        return a + b
      })
    }
  };
  this._minu = function(a) {
    return function(b) {
      return e(function() {
        return a - b
      })
    }
  };
  this._less_eq = function(a) {
    return function(b) {
      return e(function() {
        return a <= b
      })
    }
  };
  this._grea_eq = function(a) {
    return function(b) {
      return e(function() {
        return a >= b
      })
    }
  };
  this._less = function(a) {
    return function(b) {
      return e(function() {
        return a < b
      })
    }
  };
  this._grea = w;
  this._bang_eq = function(a) {
    return function(b) {
      return z(k(a)(b))
    }
  };
  this._eq_eq = k;
  this.fib = t;
  this.speedtest = new function() {
    this.time = function(a) {
      return l(function() {
        return(new Date).getTime()
      })(function() {
        return function(b) {
          return l(a)(function() {
            return function() {
              return l(function() {
                return(new Date).getTime()
              })(function() {
                return function(a) {
                  return x(a - b)
                }
              }())
            }
          }())
        }
      }())
    };
    this.fast_fib = function() {
      return e(function() {
        function a(b) {
          return 0 === b ? 0 : 1 === b ? 1 : a(b - 1) + a(b - 2)
        }
        return a
      })
    }();
    this.floor = function() {
      return e(function() {
        return Math.floor
      })
    }()
  };
  this._less_or = y;
  this._or_grea = function(a) {
    return function(b) {
      return b(a)
    }
  };
  this._less_col = function(a) {
    return function(b) {
      return a(b)
    }
  };
  this._comp_col = q;
  this.id = function(a) {
    return a
  };
  this.flip = function(a) {
    return function(b) {
      return function(c) {
        return a(c)(b)
      }
    }
  };
  this.err = function(a) {
    return e(function() {
      try {
        return a(), !1
      }catch(b) {
        return b
      }
    })
  };
  this.option = function(a) {
    return function(b) {
      if(b.hasOwnProperty("nil")) {
        return a
      }
      var c, f;
      if(f = b.hasOwnProperty("some")) {
        c = b.some, f = g
      }
      if(f) {
        return c
      }
      args = [];
      v()
    }
  };
  var o = new function() {
    function a(a) {
      return function(p) {
        return function() {
          var b = function() {
            return function(m) {
              if(m.hasOwnProperty("nil")) {
                return p
              }
              var e, c, f;
              if(f = m.hasOwnProperty("head") && m.hasOwnProperty("tail")) {
                e = m.head, c = m.tail, f = g
              }
              if(f) {
                return a(e)(b(c))
              }
              args = [];
              v()
            }
          }();
          return b
        }()
      }
    }
    function b(a) {
      return function(b) {
        if(b.hasOwnProperty("nil")) {
          throw"Foldl1 called on empty list";
        }
        return c(a)(i(b))(j(b))
      }
    }
    function c(a) {
      return function(b) {
        return function(d) {
          if(g) {
            return function() {
              var m = function() {
                return function(b) {
                  return function(p) {
                    var d;
                    d = b;
                    if(p.hasOwnProperty("nil")) {
                      return d
                    }
                    var e, c;
                    d = b;
                    var f;
                    if(f = p.hasOwnProperty("head") && p.hasOwnProperty("tail")) {
                      e = p.head, c = p.tail, f = g
                    }
                    if(f) {
                      return m(a(d)(e))(c)
                    }
                    args = [];
                    v()
                  }
                }
              }();
              return m(b)(d)
            }()
          }
          args = [];
          v()
        }
      }
    }
    function f(a) {
      return function(b) {
        return b.hasOwnProperty("nil") ? {nil:g} : n(a(i(b)))(f(a)(j(b)))
      }
    }
    function e(a) {
      return function(b) {
        var d;
        if(b.hasOwnProperty("nil")) {
          return{nil:g}
        }
        var c;
        d = a;
        c = b;
        if(d(i(c))) {
          return n(i(c))(e(d)(j(c)))
        }
        d = a;
        return y(e(d))(j(b))
      }
    }
    function l(a) {
      return function(b) {
        var d, c;
        if(c = a.hasOwnProperty("nil")) {
          d = b, c = g
        }
        if(c) {
          return d
        }
        var e, f;
        if(c = a.hasOwnProperty("head") && a.hasOwnProperty("tail")) {
          d = a.head, e = a.tail, c = g
        }
        c && (f = b, c = g);
        if(c) {
          return n(d)(l(e)(f))
        }
        args = [];
        v()
      }
    }
    function o(a) {
      return function(b) {
        return 0 === a ? {nil:g} : n(b)(o(a - 1)(b))
      }
    }
    function u(a) {
      if(a.hasOwnProperty("nil")) {
        return 0
      }
      var b, d;
      if(d = a.hasOwnProperty("tail")) {
        b = a.tail, d = g
      }
      if(d) {
        return 1 + u(b)
      }
      args = [];
      v()
    }
    function s(a) {
      return function(b) {
        if(b.hasOwnProperty("nil")) {
          return{nil:g}
        }
        var d;
        d = b;
        return a(i(d)) ? s(a)(j(d)) : b
      }
    }
    function A(a) {
      return function(b) {
        var d, c;
        if(c = 0 === a) {
          d = b, c = g
        }
        return c ? d : A(a - 1)(j(b))
      }
    }
    function B(a) {
      return function(b) {
        return 0 === a ? {nil:g} : n(i(b))(B(a - 1)(j(b)))
      }
    }
    function C(a) {
      var b, d;
      if(d = a.hasOwnProperty("head") && a.hasOwnProperty("tail")) {
        b = a.head, d = a.tail.hasOwnProperty("nil")
      }
      if(d) {
        return b
      }
      if(d = a.hasOwnProperty("tail")) {
        b = a.tail, d = g
      }
      if(d) {
        return C(b)
      }
      if(a.hasOwnProperty("nil")) {
        throw"Last called on empty list";
      }
      args = [];
      v()
    }
    function j(a) {
      var b, d;
      if(d = a.hasOwnProperty("tail")) {
        b = a.tail, d = g
      }
      if(d) {
        return b
      }
      if(a.hasOwnProperty("nil")) {
        throw"Tail called on empty list";
      }
      args = [];
      v()
    }
    function i(a) {
      var b, d;
      if(d = a.hasOwnProperty("head")) {
        b = a.head, d = g
      }
      if(d) {
        return b
      }
      if(a.hasOwnProperty("nil")) {
        throw"Head called on empty list";
      }
      args = [];
      v()
    }
    function n(a) {
      return function(b) {
        return{head:a, tail:b}
      }
    }
    this._col_col = n;
    this.empty_ = function(a) {
      return a.hasOwnProperty("nil") ? g : !1
    };
    this.head = i;
    this.tail = j;
    this.last = C;
    this.take = B;
    this.drop = A;
    this.take_while = function(a) {
      return function(b) {
        return b.hasOwnProperty("nil") ? {nil:g} : a(i(b)) ? n(i(b))(e(a)(j(b))) : {nil:g}
      }
    };
    this.drop_while = s;
    this.length = u;
    this.init = o;
    this._plus_plus = l;
    this.filter = e;
    this.map = f;
    this.reverse = function() {
      return function() {
        function a(b) {
          return function(d) {
            var c;
            c = b;
            if(d.hasOwnProperty("nil")) {
              return c
            }
            var e, f;
            c = b;
            var s;
            if(s = d.hasOwnProperty("head") && d.hasOwnProperty("tail")) {
              e = d.head, f = d.tail, s = g
            }
            if(s) {
              return a(n(e)(c))(f)
            }
            args = [];
            v()
          }
        }
        return a({nil:g})
      }()
    }();
    this.foldl = c;
    this.foldl1 = b;
    this.foldr = a;
    this.foldr1 = function(b) {
      return function(c) {
        if(c.hasOwnProperty("nil")) {
          throw"Foldr1 called on empty list";
        }
        return a(b)(i(c))(j(c))
      }
    };
    this.all_ = function(a) {
      return q(b(function() {
        return function(a) {
          return function(b) {
            return a && b
          }
        }
      }()))(f(a))
    };
    this.any_ = function(a) {
      return q(b(function() {
        return function(a) {
          return function(b) {
            return a || b
          }
        }
      }()))(f(a))
    };
    this.sum = function() {
      return b(function(a) {
        return function(b) {
          return a + b
        }
      })
    }();
    this.product = function() {
      return b(function(a) {
        return function(b) {
          return a * b
        }
      })
    }();
    var k = function() {
      return b(function(a) {
        return function(b) {
          return l(a)(b)
        }
      })
    }();
    this.concat = k;
    this.concat_map = function(a) {
      return function(b) {
        return k(f(a)(b))
      }
    };
    this.maximum = function() {
      return b(function(a) {
        return function(b) {
          return w(a)(b) ? a : b
        }
      })
    }();
    this.minimum = function() {
      return b(function(a) {
        return function(b) {
          return w(a)(b) ? b : a
        }
      })
    }()
  };
  this.list = o;
  this.sequence = new function() {
    function a(a) {
      return function(b) {
        return b(a)
      }
    }
    function b(a) {
      return function(c) {
        return function() {
          return function() {
            return{next:b(a)(a(c)), val:c}
          }
        }()
      }
    }
    function c(a) {
      return a.hasOwnProperty("nil") ? function() {
        return function() {
          return{end:g}
        }
      }() : function() {
        return function() {
          return{next:c(t(a)), val:k(a)}
        }
      }()
    }
    var f = o._col_col, k = o.head, t = o.tail, r = o.take;
    this.from_list = c;
    var u = function() {
      return function() {
        return q(function(a) {
          var b, c, e;
          if(e = a.hasOwnProperty("next") && a.hasOwnProperty("val")) {
            b = a.next, c = a.val, e = g
          }
          if(e) {
            return f(c)(u(b))
          }
          if(a.hasOwnProperty("end")) {
            return{nil:g}
          }
          args = [];
          v()
        })(e)
      }()
    }();
    this.to_list = u;
    this.iterate = b;
    this.match = a;
    this.take = r = function(b) {
      return function(c) {
        return 0 === b ? function() {
          return function() {
            return{end:g}
          }
        }() : l(c)(function() {
          return function(c) {
            return x(a(c)(function() {
              return function(a) {
                if(a.hasOwnProperty("end")) {
                  return{end:g}
                }
                var c, e, f;
                if(f = a.hasOwnProperty("next") && a.hasOwnProperty("val")) {
                  c = a.next, e = a.val, f = g
                }
                if(f) {
                  return{next:r(b - 1)(c), val:e}
                }
                args = [];
                v()
              }
            }()))
          }
        }())
      }
    }
  };
  this.map = new function() {
    this._perc = function(a) {
      return function(b) {
        return{key:a, val:b}
      }
    }
  }
};
("undefined" == typeof global ? window : global).prelude = D;

