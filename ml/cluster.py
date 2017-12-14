#! /opt/homebrew/bin/python3

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


n_points = 20
penetration = 0.1

x = numpy.random.rand(n_points)
y = numpy.random.rand(n_points)
cluster = split_two_classes(cluster_fn, x, y, penetration)
plt.subplot(1, 1, 1)
plt.plot(x[cluster == 0], y[cluster == 0], 'g.')
plt.plot(x[cluster == 1], y[cluster == 1], 'r.')
plt.title('data')
plt.show()


class euclid_dist:
    def eval(x):
        return numpy.linalg.norm(x)

    def backprop(x):
        return x / numpy.linalg.norm(x)


class vector_diff:
    def eval(self, x):
        return


class linear_form:
    def eval(x):
        return numpy.dot(x, y)

    def backprop():
        return


w = numpy.array([-1.0, 1.0])
for _ in range(100):
    pass
