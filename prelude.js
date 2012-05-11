var d = !0, e = !1;
function j(q) {
  return q instanceof Array
}
function n() {
  throw"Pattern Match Exhausted";
}
function y(q) {
  return result = "undefined" != typeof q
}
var J = new function() {
  function q(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return function() {
          return function(a) {
            var b;
            if(typeof a != "undefined") {
              b = a;
              a = d
            }else {
              a = e
            }
            if(a) {
              return g(h(b))
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
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return g(h)
      }
      args = [];
      n()
    }
  }
  function t(b) {
    if(0 === b) {
      return 0
    }
    if(1 === b) {
      return 1
    }
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
    if(b) {
      return t(c - 1) + t(c - 2)
    }
    args = [];
    n()
  }
  function u(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a && z(g) && z(h)) {
        return function() {
          var a = d;
          return k(function() {
            for(var b in g) {
              a = a && u(g[b])(h[b])
            }
            b = Object.keys(g).length === Object.keys(h).length;
            return a && b
          })
        }()
      }
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a && j(g) && j(h)) {
        return function() {
          var a = d;
          return k(function() {
            for(var b in g) {
              a = a && u(g[b])(h[b])
            }
            return a && g.length == h.length
          })
        }()
      }
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g === h
        })
      }
      args = [];
      n()
    }
  }
  function A(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g > h
        })
      }
      args = [];
      n()
    }
  }
  function B(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
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
  function r(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return function() {
          return function() {
            return k(h(k(g)))
          }
        }()
      }
      args = [];
      n()
    }
  }
  function k(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
    if(b) {
      return c(null)
    }
    args = [];
    n()
  }
  function D(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
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
    "undefined" != typeof b ? (c = b, b = d) : b = e;
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
  this._grea_grea_eq = r;
  this.ret = B;
  this._grea_grea = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return function() {
          g();
          return h()
        }
      }
      args = [];
      n()
    }
  };
  this.log = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
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
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g && h
        })
      }
      args = [];
      n()
    }
  };
  this._or_or = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g || h
        })
      }
      args = [];
      n()
    }
  };
  this._star = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g * h
        })
      }
      args = [];
      n()
    }
  };
  this._forw = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g / h
        })
      }
      args = [];
      n()
    }
  };
  this._plus = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g + h
        })
      }
      args = [];
      n()
    }
  };
  this._minu = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g - h
        })
      }
      args = [];
      n()
    }
  };
  this._less_eq = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g <= h
        })
      }
      args = [];
      n()
    }
  };
  this._grea_eq = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g >= h
        })
      }
      args = [];
      n()
    }
  };
  this._less = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return k(function() {
          return g < h
        })
      }
      args = [];
      n()
    }
  };
  this._grea = A;
  this._bang_eq = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return D(u(g)(h))
      }
      args = [];
      n()
    }
  };
  this._eq_eq = u;
  this.fib = t;
  this.speedtest = new function() {
    this.time = function(b) {
      var c;
      "undefined" != typeof b ? (c = b, b = d) : b = e;
      if(b) {
        return r(function() {
          return(new Date).getTime()
        })(function() {
          return function(b) {
            var h;
            if(typeof b != "undefined") {
              h = b;
              b = d
            }else {
              b = e
            }
            if(b) {
              return r(c)(function() {
                return function() {
                  return r(function() {
                    return(new Date).getTime()
                  })(function() {
                    return function(a) {
                      var b;
                      if(typeof a != "undefined") {
                        b = a;
                        a = d
                      }else {
                        a = e
                      }
                      if(a) {
                        return B(b - h)
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
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return h(g)
      }
      args = [];
      n()
    }
  };
  this._less_col = function(b) {
    return function(c) {
      var g, h, a;
      "undefined" != typeof b ? (g = b, a = d) : a = e;
      a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
      if(a) {
        return g(h)
      }
      args = [];
      n()
    }
  };
  this._comp_col = q;
  this.id = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
    if(b) {
      return c
    }
    args = [];
    n()
  };
  this.flip = function(b) {
    return function(c) {
      return function(g) {
        var h, a, k, m;
        "undefined" != typeof b ? (h = b, m = d) : m = e;
        if(m && ("undefined" != typeof c ? (a = c, m = d) : m = e, m)) {
          "undefined" != typeof g ? (k = g, m = d) : m = e
        }
        if(m) {
          return h(k)(a)
        }
        args = [];
        n()
      }
    }
  };
  this.err = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = e;
    if(b) {
      return k(function() {
        try {
          c();
          return e
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
      var g, h;
      "undefined" != typeof b ? (g = b, h = d) : h = e;
      if(h && c.nil) {
        return g
      }
      if(g = y(c)) {
        "undefined" != typeof c.some ? (x = c.some, g = d) : g = e
      }
      if(g) {
        return x
      }
      args = [];
      n()
    }
  };
  var w = new function() {
    function b(a) {
      return function(b) {
        var l, i, f;
        "undefined" != typeof a ? (l = a, f = d) : f = e;
        f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
        if(f) {
          return function() {
            var a = function() {
              return function(b) {
                if(b.nil) {
                  return i
                }
                var f, v, c;
                if(c = y(b)) {
                  if(typeof b.head != "undefined") {
                    f = b.head;
                    c = d
                  }else {
                    c = e
                  }
                  if(c) {
                    if(typeof b.tail != "undefined") {
                      v = b.tail;
                      c = d
                    }else {
                      c = e
                    }
                  }
                }
                if(c) {
                  return l(f)(a(v))
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
        var c, i, f;
        "undefined" != typeof a ? (c = a, f = d) : f = e;
        f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
        if(f) {
          return g(c)(o(i))(p(i))
        }
        args = [];
        n()
      }
    }
    function g(a) {
      return function(b) {
        return function(c) {
          var i, f, g, h;
          "undefined" != typeof a ? (i = a, h = d) : h = e;
          if(h && ("undefined" != typeof b ? (f = b, h = d) : h = e, h)) {
            "undefined" != typeof c ? (g = c, h = d) : h = e
          }
          if(h) {
            return function() {
              var a = function() {
                return function(b) {
                  return function(f) {
                    var c, l;
                    if(typeof b != "undefined") {
                      c = b;
                      l = d
                    }else {
                      l = e
                    }
                    if(l && f.nil) {
                      return c
                    }
                    var v, h;
                    if(typeof b != "undefined") {
                      c = b;
                      l = d
                    }else {
                      l = e
                    }
                    if(l && (l = y(f))) {
                      if(typeof f.head != "undefined") {
                        v = f.head;
                        l = d
                      }else {
                        l = e
                      }
                      if(l) {
                        if(typeof f.tail != "undefined") {
                          h = f.tail;
                          l = d
                        }else {
                          l = e
                        }
                      }
                    }
                    if(l) {
                      return a(i(c)(v))(h)
                    }
                    args = [];
                    n()
                  }
                }
              }();
              return a(f)(g)
            }()
          }
          args = [];
          n()
        }
      }
    }
    function h(a) {
      return function(b) {
        var c, i;
        "undefined" != typeof a ? (c = a, i = d) : i = e;
        if(i && b.nil) {
          return{nil:d}
        }
        var f;
        "undefined" != typeof a ? (c = a, i = d) : i = e;
        i && ("undefined" != typeof b ? (f = b, i = d) : i = e);
        if(i) {
          return s(c(o(f)))(h(c)(p(f)))
        }
        args = [];
        n()
      }
    }
    function a(b) {
      return function(c) {
        var l, i;
        "undefined" != typeof b ? (l = b, i = d) : i = e;
        if(i && c.nil) {
          return{nil:d}
        }
        var f;
        "undefined" != typeof b ? (l = b, i = d) : i = e;
        i && ("undefined" != typeof c ? (f = c, i = d) : i = e);
        if(i && l(o(f))) {
          return s(o(f))(a(l)(p(f)))
        }
        "undefined" != typeof b ? (l = b, i = d) : i = e;
        i && ("undefined" != typeof c ? (f = c, i = d) : i = e);
        if(i) {
          return C(a(l))(p(f))
        }
        args = [];
        n()
      }
    }
    function k(a) {
      return function(b) {
        var c, i;
        if(i = a.nil) {
          "undefined" != typeof b ? (c = b, i = d) : i = e
        }
        if(i) {
          return c
        }
        var f, h;
        if(i = y(a)) {
          if("undefined" != typeof a.head ? (c = a.head, i = d) : i = e, i) {
            "undefined" != typeof a.tail ? (f = a.tail, i = d) : i = e
          }
        }
        i && ("undefined" != typeof b ? (h = b, i = d) : i = e);
        if(i) {
          return s(c)(k(f)(h))
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
        var c, i, f;
        "undefined" != typeof a ? (c = a, f = d) : f = e;
        f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
        if(f) {
          return s(i)(m(c - 1)(i))
        }
        args = [];
        n()
      }
    }
    function r(a) {
      if(a.nil) {
        return 0
      }
      var b, c;
      if(c = y(a)) {
        "undefined" != typeof a.tail ? (b = a.tail, c = d) : c = e
      }
      if(c) {
        return 1 + r(b)
      }
      args = [];
      n()
    }
    function E(a) {
      return function(b) {
        var c, i;
        "undefined" != typeof a ? (c = a, i = d) : i = e;
        if(i && b.nil) {
          return{nil:d}
        }
        var f;
        "undefined" != typeof a ? (c = a, i = d) : i = e;
        i && ("undefined" != typeof b ? (f = b, i = d) : i = e);
        if(i && c(o(f))) {
          return E(c)(p(f))
        }
        "undefined" != typeof b ? (f = b, b = d) : b = e;
        if(b) {
          return f
        }
        args = [];
        n()
      }
    }
    function F(a) {
      return function(b) {
        var c, i;
        if(i = 0 === a) {
          "undefined" != typeof b ? (c = b, i = d) : i = e
        }
        if(i) {
          return c
        }
        var f;
        "undefined" != typeof a ? (f = a, i = d) : i = e;
        i && ("undefined" != typeof b ? (c = b, i = d) : i = e);
        if(i) {
          return F(f - 1)(p(c))
        }
        args = [];
        n()
      }
    }
    function G(a) {
      return function(b) {
        if(0 === a) {
          return{nil:d}
        }
        var c, i, f;
        "undefined" != typeof a ? (c = a, f = d) : f = e;
        f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
        if(f) {
          return s(o(i))(G(c - 1)(p(i)))
        }
        args = [];
        n()
      }
    }
    function H(a) {
      var b, c;
      if(c = y(a)) {
        "undefined" != typeof a.head ? (b = a.head, c = d) : c = e, c = c && a.tail.nil
      }
      if(c) {
        return b
      }
      if(c = y(a)) {
        "undefined" != typeof a.tail ? (b = a.tail, c = d) : c = e
      }
      if(c) {
        return H(b)
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
        "undefined" != typeof a.tail ? (b = a.tail, c = d) : c = e
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
        "undefined" != typeof a.head ? (b = a.head, c = d) : c = e
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
    function s(a) {
      return function(b) {
        var c, i, f;
        "undefined" != typeof a ? (c = a, f = d) : f = e;
        f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
        if(f) {
          return{head:c, tail:i}
        }
        args = [];
        n()
      }
    }
    this._col_col = s;
    this.empty_ = function(a) {
      return a.nil ? d : e
    };
    this.head = o;
    this.tail = p;
    this.last = H;
    this.take = G;
    this.drop = F;
    this.take_while = function(b) {
      return function(c) {
        var h, i;
        "undefined" != typeof b ? (h = b, i = d) : i = e;
        if(i && c.nil) {
          return{nil:d}
        }
        var f;
        "undefined" != typeof b ? (h = b, i = d) : i = e;
        i && ("undefined" != typeof c ? (f = c, i = d) : i = e);
        return i && h(o(f)) ? s(o(f))(a(h)(p(f))) : {nil:d}
      }
    };
    this.drop_while = E;
    this.length = r;
    this.init = m;
    this._plus_plus = k;
    this.filter = a;
    this.map = h;
    this.reverse = function() {
      return function() {
        function a(b) {
          return function(c) {
            var i, f;
            "undefined" != typeof b ? (i = b, f = d) : f = e;
            if(f && c.nil) {
              return i
            }
            var h, g;
            "undefined" != typeof b ? (i = b, f = d) : f = e;
            if(f && (f = y(c))) {
              if("undefined" != typeof c.head ? (h = c.head, f = d) : f = e, f) {
                "undefined" != typeof c.tail ? (g = c.tail, f = d) : f = e
              }
            }
            if(f) {
              return a(s(h)(i))(g)
            }
            args = [];
            n()
          }
        }
        return a({nil:d})
      }()
    }();
    this.foldl = g;
    this.foldl1 = c;
    this.foldr = b;
    this.foldr1 = function(a) {
      return function(c) {
        if(c.nil) {
          throw"Foldr1 called on empty list";
        }
        var h, i, f;
        "undefined" != typeof a ? (h = a, f = d) : f = e;
        f && ("undefined" != typeof c ? (i = c, f = d) : f = e);
        if(f) {
          return b(h)(o(i))(p(i))
        }
        args = [];
        n()
      }
    };
    this.all_ = function(a) {
      var b;
      "undefined" != typeof a ? (b = a, a = d) : a = e;
      if(a) {
        return q(c(function() {
          return function(a) {
            return function(b) {
              var c, h, g;
              if(typeof a != "undefined") {
                c = a;
                g = d
              }else {
                g = e
              }
              if(g) {
                if(typeof b != "undefined") {
                  h = b;
                  g = d
                }else {
                  g = e
                }
              }
              if(g) {
                return c && h
              }
              args = [];
              n()
            }
          }
        }()))(h(b))
      }
      args = [];
      n()
    };
    this.any_ = function(a) {
      var b;
      "undefined" != typeof a ? (b = a, a = d) : a = e;
      if(a) {
        return q(c(function() {
          return function(a) {
            return function(b) {
              var c, h, g;
              if(typeof a != "undefined") {
                c = a;
                g = d
              }else {
                g = e
              }
              if(g) {
                if(typeof b != "undefined") {
                  h = b;
                  g = d
                }else {
                  g = e
                }
              }
              if(g) {
                return c || h
              }
              args = [];
              n()
            }
          }
        }()))(h(b))
      }
      args = [];
      n()
    };
    this.sum = function() {
      return c(function(a) {
        return function(b) {
          var c, i, f;
          "undefined" != typeof a ? (c = a, f = d) : f = e;
          f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
          if(f) {
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
          var c, i, f;
          "undefined" != typeof a ? (c = a, f = d) : f = e;
          f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
          if(f) {
            return c * i
          }
          args = [];
          n()
        }
      })
    }();
    var I = function() {
      return c(function(a) {
        return function(b) {
          var c, i, f;
          "undefined" != typeof a ? (c = a, f = d) : f = e;
          f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
          if(f) {
            return k(c)(i)
          }
          args = [];
          n()
        }
      })
    }();
    this.concat = I;
    this.concat_map = function(a) {
      return function(b) {
        var c, i, f;
        "undefined" != typeof a ? (c = a, f = d) : f = e;
        f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
        if(f) {
          return I(h(c)(i))
        }
        args = [];
        n()
      }
    };
    this.maximum = function() {
      return c(function(a) {
        return function(b) {
          var c, i, f;
          "undefined" != typeof a ? (c = a, f = d) : f = e;
          f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
          if(f) {
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
          var c, i, f;
          "undefined" != typeof a ? (c = a, f = d) : f = e;
          f && ("undefined" != typeof b ? (i = b, f = d) : f = e);
          if(f) {
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
        var c, h, g;
        "undefined" != typeof a ? (c = a, g = d) : g = e;
        g && ("undefined" != typeof b ? (h = b, g = d) : g = e);
        if(g) {
          return h(c)
        }
        args = [];
        n()
      }
    }
    function c(a) {
      return function(b) {
        var h, g, k;
        "undefined" != typeof a ? (h = a, k = d) : k = e;
        k && ("undefined" != typeof b ? (g = b, k = d) : k = e);
        if(k) {
          return function() {
            return function() {
              return{next:c(h)(h(g)), val:g}
            }
          }()
        }
        args = [];
        n()
      }
    }
    function g(b) {
      if(b.nil) {
        return function() {
          return function() {
            return{end:d}
          }
        }()
      }
      var c;
      "undefined" != typeof b ? (c = b, b = d) : b = e;
      if(b) {
        return function() {
          return function() {
            return{next:g(u(c)), val:a(c)}
          }
        }()
      }
      args = [];
      n()
    }
    var h = w._col_col, a = w.head, u = w.tail, m = w.take;
    this.from_list = g;
    var t = function() {
      return function() {
        return q(function(a) {
          var b, c, g;
          if(g = y(a)) {
            if("undefined" != typeof a.next ? (b = a.next, g = d) : g = e, g) {
              "undefined" != typeof a.val ? (c = a.val, g = d) : g = e
            }
          }
          if(g) {
            return h(c)(t(b))
          }
          if(a.end) {
            return{nil:d}
          }
          args = [];
          n()
        })(k)
      }()
    }();
    this.to_list = t;
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
        "undefined" != typeof a ? (h = a, k = d) : k = e;
        k && ("undefined" != typeof c ? (g = c, k = d) : k = e);
        if(k) {
          return r(g)(function() {
            return function(a) {
              var c;
              if(typeof a != "undefined") {
                c = a;
                a = d
              }else {
                a = e
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
                        g = e
                      }
                      if(g) {
                        if(typeof a.val != "undefined") {
                          c = a.val;
                          g = d
                        }else {
                          g = e
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
        var g, h, a;
        "undefined" != typeof b ? (g = b, a = d) : a = e;
        a && ("undefined" != typeof c ? (h = c, a = d) : a = e);
        if(a) {
          return{key:g, val:h}
        }
        args = [];
        n()
      }
    }
  }
};
("undefined" == typeof global ? window : global).prelude = J;

