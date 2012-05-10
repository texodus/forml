var d = !0, f = !1;
function j(q) {
  return q instanceof Array
}
function n() {
  throw"Pattern Match Exhausted";
}
function y(q) {
  return result = "undefined" != typeof q
}
var K = new function() {
  function q(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return function() {
          return function(a) {
            var b;
            if(typeof a != "undefined") {
              b = a;
              a = d
            }else {
              a = f
            }
            if(a) {
              return h(g(b))
            }
            args = [];
            n()
          }
        }()
      }
      args = [];
      n()
    }
  }
  function C(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return h(g)
      }
      args = [];
      n()
    }
  }
  function s(b) {
    if(0 === b) {
      return 0
    }
    if(1 === b) {
      return 1
    }
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return s(c - 1) + s(c - 2)
    }
    args = [];
    n()
  }
  function t(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a && z(h) && z(g)) {
        return function() {
          var a = d;
          return k(function() {
            for(var b in h) {
              a = a && t(h[b])(g[b])
            }
            return a
          })
        }()
      }
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a && j(h) && j(g)) {
        return function() {
          var a = d;
          return k(function() {
            for(var b in h) {
              a = a && t(h[b])(g[b])
            }
            return a && h.length == g.length
          })
        }()
      }
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h === g
        })
      }
      args = [];
      n()
    }
  }
  function A(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h > g
        })
      }
      args = [];
      n()
    }
  }
  function B(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return function() {
        return function() {
          return c
        }
      }()
    }
    args = [];
    n()
  }
  function u(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return function() {
          return function() {
            return k(g(k(h)))
          }
        }()
      }
      args = [];
      n()
    }
  }
  function k(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return c(null)
    }
    args = [];
    n()
  }
  function D(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return k(function() {
        return!c
      })
    }
    args = [];
    n()
  }
  function z(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return k(function() {
        return typeof c === "object"
      })
    }
    args = [];
    n()
  }
  this.object_ = z;
  this.array_ = j;
  this.not = D;
  this.run = k;
  this._grea_grea_eq = u;
  this.ret = B;
  this._grea_grea = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return function() {
          h();
          return g()
        }
      }
      args = [];
      n()
    }
  };
  this.log = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return function() {
        return console.log(c)
      }
    }
    args = [];
    n()
  };
  this._and_and = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h && g
        })
      }
      args = [];
      n()
    }
  };
  this._or_or = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h || g
        })
      }
      args = [];
      n()
    }
  };
  this._star = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h * g
        })
      }
      args = [];
      n()
    }
  };
  this._forw = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h / g
        })
      }
      args = [];
      n()
    }
  };
  this._plus = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h + g
        })
      }
      args = [];
      n()
    }
  };
  this._minu = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h - g
        })
      }
      args = [];
      n()
    }
  };
  this._less_eq = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h <= g
        })
      }
      args = [];
      n()
    }
  };
  this._grea_eq = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h >= g
        })
      }
      args = [];
      n()
    }
  };
  this._less = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return k(function() {
          return h < g
        })
      }
      args = [];
      n()
    }
  };
  this._grea = A;
  this._eq_eq = t;
  this._bang_eq = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return D(t(h)(g))
      }
      args = [];
      n()
    }
  };
  this.fib = s;
  this.speedtest = new function() {
    this.time = function(b) {
      var c;
      "undefined" != typeof b ? (c = b, b = d) : b = f;
      if(b) {
        return u(function() {
          return(new Date).getTime()
        })(function() {
          return function(b) {
            var g;
            if(typeof b != "undefined") {
              g = b;
              b = d
            }else {
              b = f
            }
            if(b) {
              return u(c)(function() {
                return function() {
                  return u(function() {
                    return(new Date).getTime()
                  })(function() {
                    return function(a) {
                      var b;
                      if(typeof a != "undefined") {
                        b = a;
                        a = d
                      }else {
                        a = f
                      }
                      if(a) {
                        return B(b - g)
                      }
                      args = [];
                      n()
                    }
                  }())
                }
              }())
            }
            args = [];
            n()
          }
        }())
      }
      args = [];
      n()
    };
    this.fast_fib = function() {
      return k(function() {
        function b(c) {
          return 0 === c ? 0 : 1 === c ? 1 : b(c - 1) + b(c - 2)
        }
        return b
      })
    }();
    this.floor = function() {
      return k(function() {
        return Math.floor
      })
    }()
  };
  this._less_or = C;
  this._or_grea = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return g(h)
      }
      args = [];
      n()
    }
  };
  this._less_col = function(b) {
    return function(c) {
      var h, g, a;
      "undefined" != typeof b ? (h = b, a = d) : a = f;
      a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
      if(a) {
        return h(g)
      }
      args = [];
      n()
    }
  };
  this._comp_col = q;
  this.id = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return c
    }
    args = [];
    n()
  };
  this.flip = function(b) {
    return function(c) {
      return function(h) {
        var g, a, k, m;
        "undefined" != typeof b ? (g = b, m = d) : m = f;
        if(m && ("undefined" != typeof c ? (a = c, m = d) : m = f, m)) {
          "undefined" != typeof h ? (k = h, m = d) : m = f
        }
        if(m) {
          return g(k)(a)
        }
        args = [];
        n()
      }
    }
  };
  this.err = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = f;
    if(b) {
      return k(function() {
        try {
          c();
          return f
        }catch(b) {
          return b
        }
      })
    }
    args = [];
    n()
  };
  this.option = function(b) {
    return function(c) {
      var h, g;
      "undefined" != typeof b ? (h = b, g = d) : g = f;
      if(g && c.nil) {
        return h
      }
      if(h = y(c)) {
        "undefined" != typeof c.some ? (x = c.some, h = d) : h = f
      }
      if(h) {
        return x
      }
      args = [];
      n()
    }
  };
  var w = new function() {
    function b(a) {
      return function(b) {
        var l, i, e;
        "undefined" != typeof a ? (l = a, e = d) : e = f;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
        if(e) {
          return function() {
            var a = function() {
              return function(b) {
                if(b.nil) {
                  return i
                }
                var e, v, c;
                if(c = y(b)) {
                  if(typeof b.head != "undefined") {
                    e = b.head;
                    c = d
                  }else {
                    c = f
                  }
                  if(c) {
                    if(typeof b.tail != "undefined") {
                      v = b.tail;
                      c = d
                    }else {
                      c = f
                    }
                  }
                }
                if(c) {
                  return l(e)(a(v))
                }
                args = [];
                n()
              }
            }();
            return a
          }()
        }
        args = [];
        n()
      }
    }
    function c(a) {
      return function(b) {
        if(b.nil) {
          throw"Foldl1 called on empty list";
        }
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = f;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
        if(e) {
          return h(c)(o(i))(p(i))
        }
        args = [];
        n()
      }
    }
    function h(a) {
      return function(b) {
        return function(c) {
          var i, e, h, g;
          "undefined" != typeof a ? (i = a, g = d) : g = f;
          if(g && ("undefined" != typeof b ? (e = b, g = d) : g = f, g)) {
            "undefined" != typeof c ? (h = c, g = d) : g = f
          }
          if(g) {
            return function() {
              var a = function() {
                return function(b) {
                  return function(e) {
                    var c, l;
                    if(typeof b != "undefined") {
                      c = b;
                      l = d
                    }else {
                      l = f
                    }
                    if(l && e.nil) {
                      return c
                    }
                    var v, E;
                    if(typeof b != "undefined") {
                      c = b;
                      l = d
                    }else {
                      l = f
                    }
                    if(l && (l = y(e))) {
                      if(typeof e.head != "undefined") {
                        v = e.head;
                        l = d
                      }else {
                        l = f
                      }
                      if(l) {
                        if(typeof e.tail != "undefined") {
                          E = e.tail;
                          l = d
                        }else {
                          l = f
                        }
                      }
                    }
                    if(l) {
                      return a(i(c)(v))(E)
                    }
                    args = [];
                    n()
                  }
                }
              }();
              return a(e)(h)
            }()
          }
          args = [];
          n()
        }
      }
    }
    function g(a) {
      return function(b) {
        var c, i;
        "undefined" != typeof a ? (c = a, i = d) : i = f;
        if(i && b.nil) {
          return{nil:d}
        }
        var e;
        "undefined" != typeof a ? (c = a, i = d) : i = f;
        i && ("undefined" != typeof b ? (e = b, i = d) : i = f);
        if(i) {
          return r(c(o(e)))(g(c)(p(e)))
        }
        args = [];
        n()
      }
    }
    function a(b) {
      return function(c) {
        var l, i;
        "undefined" != typeof b ? (l = b, i = d) : i = f;
        if(i && c.nil) {
          return{nil:d}
        }
        var e;
        "undefined" != typeof b ? (l = b, i = d) : i = f;
        i && ("undefined" != typeof c ? (e = c, i = d) : i = f);
        if(i && l(o(e))) {
          return r(o(e))(a(l)(p(e)))
        }
        "undefined" != typeof b ? (l = b, i = d) : i = f;
        i && ("undefined" != typeof c ? (e = c, i = d) : i = f);
        if(i) {
          return C(a(l))(p(e))
        }
        args = [];
        n()
      }
    }
    function k(a) {
      return function(b) {
        var c, i;
        if(i = a.nil) {
          "undefined" != typeof b ? (c = b, i = d) : i = f
        }
        if(i) {
          return c
        }
        var e, g;
        if(i = y(a)) {
          if("undefined" != typeof a.head ? (c = a.head, i = d) : i = f, i) {
            "undefined" != typeof a.tail ? (e = a.tail, i = d) : i = f
          }
        }
        i && ("undefined" != typeof b ? (g = b, i = d) : i = f);
        if(i) {
          return r(c)(k(e)(g))
        }
        args = [];
        n()
      }
    }
    function m(a) {
      return function(b) {
        if(0 === a) {
          return{nil:d}
        }
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = f;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
        if(e) {
          return r(i)(m(c - 1)(i))
        }
        args = [];
        n()
      }
    }
    function q(a) {
      if(a.nil) {
        return 0
      }
      var b, c;
      if(c = y(a)) {
        "undefined" != typeof a.tail ? (b = a.tail, c = d) : c = f
      }
      if(c) {
        return 1 + q(b)
      }
      args = [];
      n()
    }
    function F(a) {
      return function(b) {
        var c, i;
        "undefined" != typeof a ? (c = a, i = d) : i = f;
        if(i && b.nil) {
          return{nil:d}
        }
        var e;
        "undefined" != typeof a ? (c = a, i = d) : i = f;
        i && ("undefined" != typeof b ? (e = b, i = d) : i = f);
        if(i && c(o(e))) {
          return F(c)(p(e))
        }
        "undefined" != typeof b ? (e = b, b = d) : b = f;
        if(b) {
          return e
        }
        args = [];
        n()
      }
    }
    function G(a) {
      return function(b) {
        var c, i;
        if(i = 0 === a) {
          "undefined" != typeof b ? (c = b, i = d) : i = f
        }
        if(i) {
          return c
        }
        var e;
        "undefined" != typeof a ? (e = a, i = d) : i = f;
        i && ("undefined" != typeof b ? (c = b, i = d) : i = f);
        if(i) {
          return G(e - 1)(p(c))
        }
        args = [];
        n()
      }
    }
    function H(a) {
      return function(b) {
        if(0 === a) {
          return{nil:d}
        }
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = f;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
        if(e) {
          return r(o(i))(H(c - 1)(p(i)))
        }
        args = [];
        n()
      }
    }
    function I(a) {
      var b, c;
      if(c = y(a)) {
        "undefined" != typeof a.head ? (b = a.head, c = d) : c = f, c = c && a.tail.nil
      }
      if(c) {
        return b
      }
      if(c = y(a)) {
        "undefined" != typeof a.tail ? (b = a.tail, c = d) : c = f
      }
      if(c) {
        return I(b)
      }
      if(a.nil) {
        throw"Last called on empty list";
      }
      args = [];
      n()
    }
    function p(a) {
      var b, c;
      if(c = y(a)) {
        "undefined" != typeof a.tail ? (b = a.tail, c = d) : c = f
      }
      if(c) {
        return b
      }
      if(a.nil) {
        throw"Tail called on empty list";
      }
      args = [];
      n()
    }
    function o(a) {
      var b, c;
      if(c = y(a)) {
        "undefined" != typeof a.head ? (b = a.head, c = d) : c = f
      }
      if(c) {
        return b
      }
      if(a.nil) {
        throw"Head called on empty list";
      }
      args = [];
      n()
    }
    function r(a) {
      return function(b) {
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = f;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
        if(e) {
          return{head:c, tail:i}
        }
        args = [];
        n()
      }
    }
    this._col_col = r;
    this.empty_ = function(a) {
      return a.nil ? d : f
    };
    this.head = o;
    this.tail = p;
    this.last = I;
    this.take = H;
    this.drop = G;
    this.take_while = function(b) {
      return function(c) {
        var g, i;
        "undefined" != typeof b ? (g = b, i = d) : i = f;
        if(i && c.nil) {
          return{nil:d}
        }
        var e;
        "undefined" != typeof b ? (g = b, i = d) : i = f;
        i && ("undefined" != typeof c ? (e = c, i = d) : i = f);
        return i && g(o(e)) ? r(o(e))(a(g)(p(e))) : {nil:d}
      }
    };
    this.drop_while = F;
    this.length = q;
    this.init = m;
    this._plus_plus = k;
    this.filter = a;
    this.map = g;
    this.reverse = function() {
      return function() {
        function a(b) {
          return function(c) {
            var i, e;
            "undefined" != typeof b ? (i = b, e = d) : e = f;
            if(e && c.nil) {
              return i
            }
            var g, h;
            "undefined" != typeof b ? (i = b, e = d) : e = f;
            if(e && (e = y(c))) {
              if("undefined" != typeof c.head ? (g = c.head, e = d) : e = f, e) {
                "undefined" != typeof c.tail ? (h = c.tail, e = d) : e = f
              }
            }
            if(e) {
              return a(r(g)(i))(h)
            }
            args = [];
            n()
          }
        }
        return a({nil:d})
      }()
    }();
    this.foldl = h;
    this.foldl1 = c;
    this.foldr = b;
    this.foldr1 = function(a) {
      return function(c) {
        if(c.nil) {
          throw"Foldr1 called on empty list";
        }
        var g, i, e;
        "undefined" != typeof a ? (g = a, e = d) : e = f;
        e && ("undefined" != typeof c ? (i = c, e = d) : e = f);
        if(e) {
          return b(g)(o(i))(p(i))
        }
        args = [];
        n()
      }
    };
    this.all_ = function(a) {
      var b;
      "undefined" != typeof a ? (b = a, a = d) : a = f;
      if(a) {
        return c(function() {
          return function(a) {
            return function(c) {
              var e, g, h;
              if(typeof a != "undefined") {
                e = a;
                h = d
              }else {
                h = f
              }
              if(h) {
                if(typeof c != "undefined") {
                  g = c;
                  h = d
                }else {
                  h = f
                }
              }
              if(h) {
                return b(e) && b(g)
              }
              args = [];
              n()
            }
          }
        }())
      }
      args = [];
      n()
    };
    this.any_ = function(a) {
      var b;
      "undefined" != typeof a ? (b = a, a = d) : a = f;
      if(a) {
        return c(function() {
          return function(a) {
            return function(c) {
              var e, g, h;
              if(typeof a != "undefined") {
                e = a;
                h = d
              }else {
                h = f
              }
              if(h) {
                if(typeof c != "undefined") {
                  g = c;
                  h = d
                }else {
                  h = f
                }
              }
              if(h) {
                return b(e) || b(g)
              }
              args = [];
              n()
            }
          }
        }())
      }
      args = [];
      n()
    };
    this.sum = function() {
      return c(function(a) {
        return function(b) {
          var c, i, e;
          "undefined" != typeof a ? (c = a, e = d) : e = f;
          e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
          if(e) {
            return c + i
          }
          args = [];
          n()
        }
      })
    }();
    this.product = function() {
      return c(function(a) {
        return function(b) {
          var c, i, e;
          "undefined" != typeof a ? (c = a, e = d) : e = f;
          e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
          if(e) {
            return c * i
          }
          args = [];
          n()
        }
      })
    }();
    var J = function() {
      return c(function(a) {
        return function(b) {
          var c, i, e;
          "undefined" != typeof a ? (c = a, e = d) : e = f;
          e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
          if(e) {
            return k(c)(i)
          }
          args = [];
          n()
        }
      })
    }();
    this.concat = J;
    this.concat_map = function(a) {
      return function(b) {
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = f;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
        if(e) {
          return J(g(c)(i))
        }
        args = [];
        n()
      }
    };
    this.maximum = function() {
      return c(function(a) {
        return function(b) {
          var c, i, e;
          "undefined" != typeof a ? (c = a, e = d) : e = f;
          e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
          if(e) {
            return A(c)(i) ? c : i
          }
          args = [];
          n()
        }
      })
    }();
    this.minimum = function() {
      return c(function(a) {
        return function(b) {
          var c, i, e;
          "undefined" != typeof a ? (c = a, e = d) : e = f;
          e && ("undefined" != typeof b ? (i = b, e = d) : e = f);
          if(e) {
            return A(c)(i) ? i : c
          }
          args = [];
          n()
        }
      })
    }()
  };
  this.list = w;
  this.sequence = new function() {
    function b(a) {
      return function(b) {
        var c, g, h;
        "undefined" != typeof a ? (c = a, h = d) : h = f;
        h && ("undefined" != typeof b ? (g = b, h = d) : h = f);
        if(h) {
          return g(c)
        }
        args = [];
        n()
      }
    }
    function c(a) {
      return function(b) {
        var g, h, k;
        "undefined" != typeof a ? (g = a, k = d) : k = f;
        k && ("undefined" != typeof b ? (h = b, k = d) : k = f);
        if(k) {
          return function() {
            return function() {
              return{next:c(g)(g(h)), val:h}
            }
          }()
        }
        args = [];
        n()
      }
    }
    function h(b) {
      if(b.nil) {
        return function() {
          return function() {
            return{end:d}
          }
        }()
      }
      var c;
      "undefined" != typeof b ? (c = b, b = d) : b = f;
      if(b) {
        return function() {
          return function() {
            return{next:h(t(c)), val:a(c)}
          }
        }()
      }
      args = [];
      n()
    }
    var g = w._col_col, a = w.head, t = w.tail, m = w.take;
    this.from_list = h;
    var s = function() {
      return function() {
        return q(function(a) {
          var b, c, h;
          if(h = y(a)) {
            if("undefined" != typeof a.next ? (b = a.next, h = d) : h = f, h) {
              "undefined" != typeof a.val ? (c = a.val, h = d) : h = f
            }
          }
          if(h) {
            return g(c)(s(b))
          }
          if(a.end) {
            return{nil:d}
          }
          args = [];
          n()
        })(k)
      }()
    }();
    this.to_list = s;
    this.iterate = c;
    this.match = b;
    this.take = m = function(a) {
      return function(c) {
        if(0 === a) {
          return function() {
            return function() {
              return{end:d}
            }
          }()
        }
        var h, g, k;
        "undefined" != typeof a ? (h = a, k = d) : k = f;
        k && ("undefined" != typeof c ? (g = c, k = d) : k = f);
        if(k) {
          return u(g)(function() {
            return function(a) {
              var c;
              if(typeof a != "undefined") {
                c = a;
                a = d
              }else {
                a = f
              }
              if(a) {
                return B(b(c)(function() {
                  return function(a) {
                    if(a.end) {
                      return{end:d}
                    }
                    var b, c, g;
                    if(g = y(a)) {
                      if(typeof a.next != "undefined") {
                        b = a.next;
                        g = d
                      }else {
                        g = f
                      }
                      if(g) {
                        if(typeof a.val != "undefined") {
                          c = a.val;
                          g = d
                        }else {
                          g = f
                        }
                      }
                    }
                    if(g) {
                      return{next:m(h - 1)(b), val:c}
                    }
                    args = [];
                    n()
                  }
                }()))
              }
              args = [];
              n()
            }
          }())
        }
        args = [];
        n()
      }
    }
  };
  this.map = new function() {
    this._perc = function(b) {
      return function(c) {
        var h, g, a;
        "undefined" != typeof b ? (h = b, a = d) : a = f;
        a && ("undefined" != typeof c ? (g = c, a = d) : a = f);
        if(a) {
          return{key:h, val:g}
        }
        args = [];
        n()
      }
    }
  }
};
("undefined" == typeof global ? window : global).prelude = K;

