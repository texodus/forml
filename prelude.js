var d = !0, g = !1;
function j(s) {
  return s instanceof Array
}
function l() {
  throw"Pattern Match Exhausted";
}
function v(s) {
  return result = "undefined" != typeof s
}
var L = new function() {
  function s(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return function() {
          return function(a) {
            var b;
            if(typeof a != "undefined") {
              b = a;
              a = d
            }else {
              a = g
            }
            if(a) {
              return h(f(b))
            }
            args = [];
            l()
          }
        }()
      }
      args = [];
      l()
    }
  }
  function D(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return f(h)
      }
      args = [];
      l()
    }
  }
  function E(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return h(f)
      }
      args = [];
      l()
    }
  }
  function w(b) {
    if(0 === b) {
      return 0
    }
    if(1 === b) {
      return 1
    }
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return w(c - 1) + w(c - 2)
    }
    args = [];
    l()
  }
  function p(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a && A(h) && A(f)) {
        return function() {
          var a = d;
          return k(function() {
            for(var b in h) {
              a = a && p(h[b])(f[b])
            }
            return a
          })
        }()
      }
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a && j(h) && j(f)) {
        return function() {
          var a = d;
          return k(function() {
            for(var b in h) {
              a = a && p(h[b])(f[b])
            }
            return a && h.length == f.length
          })
        }()
      }
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h === f
        })
      }
      args = [];
      l()
    }
  }
  function B(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h > f
        })
      }
      args = [];
      l()
    }
  }
  function C(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return function() {
        return function() {
          return c
        }
      }()
    }
    args = [];
    l()
  }
  function t(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return function() {
          return function() {
            return k(f(k(h)))
          }
        }()
      }
      args = [];
      l()
    }
  }
  function k(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return c(null)
    }
    args = [];
    l()
  }
  function F(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return k(function() {
        return!c
      })
    }
    args = [];
    l()
  }
  function A(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return k(function() {
        return typeof c === "object"
      })
    }
    args = [];
    l()
  }
  this.object_ = A;
  this.array_ = j;
  this.not = F;
  this.run = k;
  this._grea_grea_eq = t;
  this.ret = C;
  this._grea_grea = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return function() {
          h();
          return f()
        }
      }
      args = [];
      l()
    }
  };
  this.log = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return function() {
        return console.log(c)
      }
    }
    args = [];
    l()
  };
  this.time = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return t(function() {
        return(new Date).getTime()
      })(function() {
        return function(b) {
          var f;
          if(typeof b != "undefined") {
            f = b;
            b = d
          }else {
            b = g
          }
          if(b) {
            return t(c)(function() {
              return function() {
                return t(function() {
                  return(new Date).getTime()
                })(function() {
                  return function(a) {
                    var b;
                    if(typeof a != "undefined") {
                      b = a;
                      a = d
                    }else {
                      a = g
                    }
                    if(a) {
                      return C(b - f)
                    }
                    args = [];
                    l()
                  }
                }())
              }
            }())
          }
          args = [];
          l()
        }
      }())
    }
    args = [];
    l()
  };
  this._and_and = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h && f
        })
      }
      args = [];
      l()
    }
  };
  this._or_or = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h || f
        })
      }
      args = [];
      l()
    }
  };
  this._star = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h * f
        })
      }
      args = [];
      l()
    }
  };
  this._forw = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h / f
        })
      }
      args = [];
      l()
    }
  };
  this._plus = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h + f
        })
      }
      args = [];
      l()
    }
  };
  this._minu = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h - f
        })
      }
      args = [];
      l()
    }
  };
  this._less_eq = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h <= f
        })
      }
      args = [];
      l()
    }
  };
  this._grea_eq = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h >= f
        })
      }
      args = [];
      l()
    }
  };
  this._less = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return k(function() {
          return h < f
        })
      }
      args = [];
      l()
    }
  };
  this._grea = B;
  this._eq_eq = p;
  this._bang_eq = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return F(p(h)(f))
      }
      args = [];
      l()
    }
  };
  this.fib = w;
  this.speedtest = new function() {
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
  this._less_or = E;
  this._or_grea = D;
  this._less_col = function(b) {
    return function(c) {
      var h, f, a;
      "undefined" != typeof b ? (h = b, a = d) : a = g;
      a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
      if(a) {
        return h(f)
      }
      args = [];
      l()
    }
  };
  this._comp_col = s;
  this.id = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return c
    }
    args = [];
    l()
  };
  this.flip = function(b) {
    return function(c) {
      return function(h) {
        var f, a, k, m;
        "undefined" != typeof b ? (f = b, m = d) : m = g;
        if(m && ("undefined" != typeof c ? (a = c, m = d) : m = g, m)) {
          "undefined" != typeof h ? (k = h, m = d) : m = g
        }
        if(m) {
          return f(k)(a)
        }
        args = [];
        l()
      }
    }
  };
  this.err = function(b) {
    var c;
    "undefined" != typeof b ? (c = b, b = d) : b = g;
    if(b) {
      return k(function() {
        try {
          c();
          return g
        }catch(b) {
          return b
        }
      })
    }
    args = [];
    l()
  };
  this.option = function(b) {
    return function(c) {
      var h, f;
      "undefined" != typeof b ? (h = b, f = d) : f = g;
      if(f && c.nil) {
        return h
      }
      if(h = v(c)) {
        "undefined" != typeof c.some ? (x = c.some, h = d) : h = g
      }
      if(h) {
        return x
      }
      args = [];
      l()
    }
  };
  var u = new function() {
    function b(a) {
      return function(b) {
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = g;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = g);
        if(e) {
          return function() {
            var a = function() {
              return function(b) {
                if(b.nil) {
                  return i
                }
                var e;
                if(e = v(b)) {
                  if(e = v(b.cons)) {
                    if(typeof b.cons.head != "undefined") {
                      y = b.cons.head;
                      e = d
                    }else {
                      e = g
                    }
                    if(e) {
                      if(typeof b.cons.tail != "undefined") {
                        ys = b.cons.tail;
                        e = d
                      }else {
                        e = g
                      }
                    }
                  }
                }
                if(e) {
                  return c(y)(a(ys))
                }
                args = [];
                l()
              }
            }();
            return a
          }()
        }
        args = [];
        l()
      }
    }
    function c(a) {
      return function(b) {
        if(b.nil) {
          throw"Foldl1 called on empty list";
        }
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = g;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = g);
        if(e) {
          return h(c)(q(i))(o(i))
        }
        args = [];
        l()
      }
    }
    function h(a) {
      return function(b) {
        return function(c) {
          var i, e, h, f;
          "undefined" != typeof a ? (i = a, f = d) : f = g;
          if(f && ("undefined" != typeof b ? (e = b, f = d) : f = g, f)) {
            "undefined" != typeof c ? (h = c, f = d) : f = g
          }
          if(f) {
            return function() {
              var a = function() {
                return function(b) {
                  return function(e) {
                    var c, n;
                    if(typeof b != "undefined") {
                      c = b;
                      n = d
                    }else {
                      n = g
                    }
                    if(n && e.nil) {
                      return c
                    }
                    if(typeof b != "undefined") {
                      c = b;
                      n = d
                    }else {
                      n = g
                    }
                    if(n && (n = v(e))) {
                      if(n = v(e.cons)) {
                        if(typeof e.cons.head != "undefined") {
                          z = e.cons.head;
                          n = d
                        }else {
                          n = g
                        }
                        if(n) {
                          if(typeof e.cons.tail != "undefined") {
                            zs = e.cons.tail;
                            n = d
                          }else {
                            n = g
                          }
                        }
                      }
                    }
                    if(n) {
                      return a(i(c)(z))(zs)
                    }
                    args = [];
                    l()
                  }
                }
              }();
              return a(e)(h)
            }()
          }
          args = [];
          l()
        }
      }
    }
    function f(a) {
      return function(b) {
        var c, i;
        "undefined" != typeof a ? (c = a, i = d) : i = g;
        if(i && b.nil) {
          return{nil:d}
        }
        var e;
        "undefined" != typeof a ? (c = a, i = d) : i = g;
        i && ("undefined" != typeof b ? (e = b, i = d) : i = g);
        if(i) {
          return r(c(q(e)))(f(c)(o(e)))
        }
        args = [];
        l()
      }
    }
    function a(b) {
      return function(c) {
        var f, i;
        "undefined" != typeof b ? (f = b, i = d) : i = g;
        if(i && c.nil) {
          return{nil:d}
        }
        var e;
        "undefined" != typeof b ? (f = b, i = d) : i = g;
        i && ("undefined" != typeof c ? (e = c, i = d) : i = g);
        if(i && f(q(e))) {
          return r(q(e))(a(f)(o(e)))
        }
        "undefined" != typeof b ? (f = b, i = d) : i = g;
        i && ("undefined" != typeof c ? (e = c, i = d) : i = g);
        if(i) {
          return E(a(f))(o(e))
        }
        args = [];
        l()
      }
    }
    function k(a) {
      return function(b) {
        var c, i;
        if(i = a.nil) {
          "undefined" != typeof b ? (c = b, i = d) : i = g
        }
        if(i) {
          return c
        }
        var e;
        if(i = v(a)) {
          if(i = v(a.cons)) {
            if("undefined" != typeof a.cons.head ? (c = a.cons.head, i = d) : i = g, i) {
              "undefined" != typeof a.cons.tail ? (ys = a.cons.tail, i = d) : i = g
            }
          }
        }
        i && ("undefined" != typeof b ? (e = b, i = d) : i = g);
        if(i) {
          return r(c)(k(ys)(e))
        }
        args = [];
        l()
      }
    }
    function m(a) {
      return function(b) {
        if(0 === a) {
          return{nil:d}
        }
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = g;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = g);
        if(e) {
          return r(i)(m(c - 1)(i))
        }
        args = [];
        l()
      }
    }
    function G(a) {
      if(a.nil) {
        return 0
      }
      var b;
      if(b = v(a)) {
        if(b = v(a.cons)) {
          "undefined" != typeof a.cons.tail ? (x = a.cons.tail, b = d) : b = g
        }
      }
      if(b) {
        return 1 + G(x)
      }
      args = [];
      l()
    }
    function H(a) {
      return function(b) {
        var c, i;
        if(i = 0 === a) {
          "undefined" != typeof b ? (c = b, i = d) : i = g
        }
        if(i) {
          return c
        }
        var e;
        "undefined" != typeof a ? (e = a, i = d) : i = g;
        i && ("undefined" != typeof b ? (c = b, i = d) : i = g);
        if(i) {
          return H(e - 1)(o(c))
        }
        args = [];
        l()
      }
    }
    function I(a) {
      return function(b) {
        if(0 === a) {
          return{nil:d}
        }
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = g;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = g);
        if(e) {
          return r(q(i))(I(c - 1)(o(i)))
        }
        args = [];
        l()
      }
    }
    function J(a) {
      var b;
      if(b = v(a)) {
        if(b = v(a.cons)) {
          "undefined" != typeof a.cons.head ? (x = a.cons.head, b = d) : b = g, b = b && a.cons.tail.nil
        }
      }
      if(b) {
        return x
      }
      if(b = v(a)) {
        if(b = v(a.cons)) {
          "undefined" != typeof a.cons.tail ? (x = a.cons.tail, b = d) : b = g
        }
      }
      if(b) {
        return J(x)
      }
      if(a.nil) {
        throw"Last called on empty list";
      }
      args = [];
      l()
    }
    function o(a) {
      var b;
      if(b = v(a)) {
        if(b = v(a.cons)) {
          "undefined" != typeof a.cons.tail ? (x = a.cons.tail, b = d) : b = g
        }
      }
      if(b) {
        return x
      }
      if(a.nil) {
        throw"Tail called on empty list";
      }
      args = [];
      l()
    }
    function q(a) {
      var b;
      if(b = v(a)) {
        if(b = v(a.cons)) {
          "undefined" != typeof a.cons.head ? (x = a.cons.head, b = d) : b = g
        }
      }
      if(b) {
        return x
      }
      if(a.nil) {
        throw"Head called on empty list";
      }
      args = [];
      l()
    }
    function r(a) {
      return function(b) {
        var c, i, e;
        "undefined" != typeof a ? (c = a, e = d) : e = g;
        e && ("undefined" != typeof b ? (i = b, e = d) : e = g);
        if(e) {
          return b = {}, b.cons = {head:c, tail:i}, b
        }
        args = [];
        l()
      }
    }
    this._col_col = r;
    this.empty_ = function(a) {
      return a.nil ? d : g
    };
    this.head = q;
    this.tail = o;
    this.last = J;
    this.take = I;
    this.drop = H;
    this.length = G;
    this.init = m;
    this._plus_plus = k;
    this.filter = a;
    this.map = f;
    this.reverse = function() {
      return function() {
        function a(b) {
          return function(c) {
            var i, e;
            "undefined" != typeof b ? (i = b, e = d) : e = g;
            if(e && c.nil) {
              return i
            }
            "undefined" != typeof b ? (i = b, e = d) : e = g;
            if(e && (e = v(c))) {
              if(e = v(c.cons)) {
                if("undefined" != typeof c.cons.head ? (x = c.cons.head, e = d) : e = g, e) {
                  "undefined" != typeof c.cons.tail ? (xs = c.cons.tail, e = d) : e = g
                }
              }
            }
            if(e) {
              return a(r(x)(i))(xs)
            }
            args = [];
            l()
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
        var f, i, e;
        "undefined" != typeof a ? (f = a, e = d) : e = g;
        e && ("undefined" != typeof c ? (i = c, e = d) : e = g);
        if(e) {
          return b(f)(q(i))(o(i))
        }
        args = [];
        l()
      }
    };
    this.all_ = function(a) {
      var b;
      "undefined" != typeof a ? (b = a, a = d) : a = g;
      if(a) {
        return c(function() {
          return function(a) {
            return function(c) {
              var e, f, h;
              if(typeof a != "undefined") {
                e = a;
                h = d
              }else {
                h = g
              }
              if(h) {
                if(typeof c != "undefined") {
                  f = c;
                  h = d
                }else {
                  h = g
                }
              }
              if(h) {
                return b(e) && b(f)
              }
              args = [];
              l()
            }
          }
        }())
      }
      args = [];
      l()
    };
    this.any_ = function(a) {
      var b;
      "undefined" != typeof a ? (b = a, a = d) : a = g;
      if(a) {
        return c(function() {
          return function(a) {
            return function(c) {
              var e, f, h;
              if(typeof a != "undefined") {
                e = a;
                h = d
              }else {
                h = g
              }
              if(h) {
                if(typeof c != "undefined") {
                  f = c;
                  h = d
                }else {
                  h = g
                }
              }
              if(h) {
                return b(e) || b(f)
              }
              args = [];
              l()
            }
          }
        }())
      }
      args = [];
      l()
    };
    this.sum = function() {
      return c(function(a) {
        return function(b) {
          var c, f, e;
          "undefined" != typeof a ? (c = a, e = d) : e = g;
          e && ("undefined" != typeof b ? (f = b, e = d) : e = g);
          if(e) {
            return c + f
          }
          args = [];
          l()
        }
      })
    }();
    this.product = function() {
      return c(function(a) {
        return function(b) {
          var c, f, e;
          "undefined" != typeof a ? (c = a, e = d) : e = g;
          e && ("undefined" != typeof b ? (f = b, e = d) : e = g);
          if(e) {
            return c * f
          }
          args = [];
          l()
        }
      })
    }();
    var K = function() {
      return c(function(a) {
        return function(b) {
          var c, f, e;
          "undefined" != typeof a ? (c = a, e = d) : e = g;
          e && ("undefined" != typeof b ? (f = b, e = d) : e = g);
          if(e) {
            return k(c)(f)
          }
          args = [];
          l()
        }
      })
    }();
    this.concat = K;
    this.concat_map = function(a) {
      return function(b) {
        var c, h, e;
        "undefined" != typeof a ? (c = a, e = d) : e = g;
        e && ("undefined" != typeof b ? (h = b, e = d) : e = g);
        if(e) {
          return K(f(c)(h))
        }
        args = [];
        l()
      }
    };
    this.maximum = function() {
      return c(function(a) {
        return function(b) {
          var c, f, e;
          "undefined" != typeof a ? (c = a, e = d) : e = g;
          e && ("undefined" != typeof b ? (f = b, e = d) : e = g);
          if(e) {
            return B(c)(f) ? c : f
          }
          args = [];
          l()
        }
      })
    }();
    this.minimum = function() {
      return c(function(a) {
        return function(b) {
          var c, f, e;
          "undefined" != typeof a ? (c = a, e = d) : e = g;
          e && ("undefined" != typeof b ? (f = b, e = d) : e = g);
          if(e) {
            return B(c)(f) ? f : c
          }
          args = [];
          l()
        }
      })
    }()
  };
  this.list = u;
  this.sequence = new function() {
    function b(a) {
      return function(c) {
        var f, h, k;
        "undefined" != typeof a ? (f = a, k = d) : k = g;
        k && ("undefined" != typeof c ? (h = c, k = d) : k = g);
        if(k) {
          return function() {
            return function() {
              return{next:b(f)(f(h)), val:h}
            }
          }()
        }
        args = [];
        l()
      }
    }
    function c(b) {
      if(b.nil) {
        return function() {
          return function() {
            return{end:d}
          }
        }()
      }
      var h;
      "undefined" != typeof b ? (h = b, b = d) : b = g;
      if(b) {
        return function() {
          return function() {
            return{next:c(a(h)), val:f(h)}
          }
        }()
      }
      args = [];
      l()
    }
    var h = u._col_col, f = u.head, a = u.tail, p = u.take;
    this.from_list = c;
    var m = function() {
      return function() {
        return s(function(a) {
          var b, c, f;
          if(f = v(a)) {
            if("undefined" != typeof a.next ? (b = a.next, f = d) : f = g, f) {
              "undefined" != typeof a.val ? (c = a.val, f = d) : f = g
            }
          }
          if(f) {
            return h(c)(m(b))
          }
          if(a.end) {
            return{nil:d}
          }
          args = [];
          l()
        })(k)
      }()
    }();
    this.to_list = m;
    this.iterate = b;
    this.take = p = function(a) {
      return function(b) {
        if(0 === a) {
          return function() {
            return function() {
              return{end:d}
            }
          }()
        }
        var c, f, h;
        "undefined" != typeof a ? (c = a, h = d) : h = g;
        h && ("undefined" != typeof b ? (f = b, h = d) : h = g);
        if(h) {
          return t(f)(function() {
            return function(a) {
              var b;
              if(typeof a != "undefined") {
                b = a;
                a = d
              }else {
                a = g
              }
              if(a) {
                return C(D(b)(function() {
                  return function(a) {
                    if(a.end) {
                      return{end:d}
                    }
                    var b, f, h;
                    if(h = v(a)) {
                      if(typeof a.next != "undefined") {
                        b = a.next;
                        h = d
                      }else {
                        h = g
                      }
                      if(h) {
                        if(typeof a.val != "undefined") {
                          f = a.val;
                          h = d
                        }else {
                          h = g
                        }
                      }
                    }
                    if(h) {
                      return{next:p(c - 1)(b), val:f}
                    }
                    args = [];
                    l()
                  }
                }()))
              }
              args = [];
              l()
            }
          }())
        }
        args = [];
        l()
      }
    }
  };
  this.map = new function() {
    this._perc = function(b) {
      return function(c) {
        var h, f, a;
        "undefined" != typeof b ? (h = b, a = d) : a = g;
        a && ("undefined" != typeof c ? (f = c, a = d) : a = g);
        if(a) {
          return{key:h, val:f}
        }
        args = [];
        l()
      }
    }
  }
};
("undefined" == typeof global ? window : global).prelude = L;

