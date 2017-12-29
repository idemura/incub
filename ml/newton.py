import math
import numpy
from matplotlib import pyplot as plt


class NumericException(Exception):
    pass


class polynom:
    def __init__(self, v):
        self.v = numpy.array(v)
        self.p = numpy.arange(len(v))

    def f(self, x):
        x_pow = numpy.power(numpy.full(self.v.size, x), self.p)
        return numpy.sum(x_pow * self.v)

    def d1f(self, x):
        x_pow = numpy.power(numpy.full(self.v.size, x), self.p)
        return numpy.sum(x_pow * self.v * self.p)

    def d2f(self, x):
        x_pow = numpy.power(numpy.full(self.v.size, x), self.p)
        return numpy.sum(x_pow * self.v * self.p)


class sq_root:
    def f(self, x):
        return pow(x - 1, 0.5)

    def d1f(self, x):
        return 0.5 / self.f(x)


class exp_sat:
    def f(self, x):
        # Avoid "math range error"
        if x >= 710:
            return 1.0
        e = math.exp(x)
        return (1 - e) / (1 + e)

    def d1f(self, x):
        # Avoid "math range error"
        if x >= 710:
            return 0.0
        e = math.exp(x)
        return -2 * e / pow(e + 1, 2)


def newton(ff, x0, rel_tol, max_steps=100):
    iter_count = 0
    x = x0
    while True:
        iter_count += 1
        if iter_count > max_steps:
            raise NumericException("newton: didn't converge")
        d1f = ff.d1f(x0)
        if abs(d1f) < 1e-12:
            raise NumericException("newton: derivative close to 0")
        x = x0 - ff.f(x0) / d1f
        print("x", x)
        if abs(x - x0) <= abs(x0) * rel_tol:
            print("stop in", iter_count)
            break
        x0 = x
    return x


def newton_solve(ff, x0, rel_tol=1e-3, real_root=None):
    try:
        r = newton(ff, x0, rel_tol)
        if real_root:
            print("root", r, "abs diff", abs(r - real_root),
                  "relative error", abs(r - real_root) / abs(real_root))
        else:
            print("root", r)
    except NumericException as e:
        print(e)


# newton_solve(polynom([2, -3, 1]), 3, rel_tol=1e-12, real_root=2)
# newton_solve(sq_root(), 4)
newton_solve(exp_sat(), 2, real_root=0)


def plot():
    xs = numpy.linspace(-10, 10, 60)
    ex = numpy.exp(xs)
    ys = - (1 - ex) / (1 + ex)
    plt.plot(xs, ys)
    plt.show()


plot()
