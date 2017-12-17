import numpy
import matplotlib.pyplot as plt
import os

USER = os.environ['USER']


def cluster_fn(x, y):
    return y - (2 * x + 0.1)


def split_two_classes(f, x, y, p):
    vf = numpy.vectorize(f)
    return numpy.where(
        vf(x, y) + p * numpy.random.randn(*x.shape) > 0,
        numpy.zeros(*x.shape),
        numpy.ones(*x.shape))


n_points = 6
penetration = 0.1

x = numpy.random.rand(n_points)
y = numpy.random.rand(n_points)
cluster = split_two_classes(cluster_fn, x, y, penetration)
print("xy")
print(x)
print(y)
if False:
    plt.subplot(1, 1, 1)
    plt.plot(x[cluster == 0], y[cluster == 0], 'g.')
    plt.plot(x[cluster == 1], y[cluster == 1], 'r.')
    plt.title('data')
    plt.show()


w = numpy.random.randn(2) * 0.25
m = numpy.array([x, y])
print(m)

lvalues = []
STEP = 0.001
for i in range(5):
    # Lose function
    d = numpy.dot(w, m) - cluster
    print("d")
    print(d)
    l = numpy.linalg.norm(d)
    print("weights", w, "lose fn", l)
    print("t w")
    print(w)
    print("-->", w.reshape(w.size, 1))
    w.reshape(w.size, 1)
    print("w", w)
    print(numpy.dot(w, d))
    print("===")
    lvalues.append(l)
    g = numpy.sum(m, axis=1)

# sqrt(sum[i](W `dot` x[i]))
#

class linear_fn:
    def __init__(self, xs, ys):
        pass

def lose_fn(w, x, c):
    wt = numpy.transpose(numpy.array([w]))
    return numpy.linalg.norm(numpy.hstack(numpy.dot(x, wt)) - c)


def lose_fn_grad(w, x, c):
    l = lose_fn(w, x, c)
    (2 * ()) / (2 * l)
    wt = numpy.transpose(numpy.array([w]))
    return numpy.linalg.norm(numpy.hstack(numpy.dot(x, wt)) - c)

def test():
    a = numpy.arange(8).reshape(2, 4)
    w = numpy.array([1, -2])
    print(a)
    print("sum")
    print(numpy.sum(a, axis=1))
    # print(numpy.dot(w, numpy.transpose(a)))
    # print(lose_fn(w, a, numpy.zeros(4)))


w = numpy.array([-1.0, 1.0])
for _ in range(100):
    pass

test()
