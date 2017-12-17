import numpy
# import scipy
import matplotlib.pyplot as plt
import os

USER = os.environ['USER']

"""
@ndarray members:
    @dtype
        item type (float64, int32, int64, ...)
    @ndim
        number of dimensions (size of @shape tuple)
    @shape
        tuple of dimension sizes: (8,), (2, 3), ...
    @size
        total number of items if we linearize the array
    @itemsize
        item size in bytes
"""

def prepare_data():
    x = numpy.linspace(0, 100, 250)
    y = 2 * x + 3
    x_shuffle = numpy.random.randn(x.size) * 1
    y_shuffle = numpy.random.randn(y.size) * 5
    return x + x_shuffle, y + y_shuffle

def plot_norm_line():
    x, y = prepare_data()
    plt.subplot(1, 1, 1)
    plt.plot(x, y, 'g.')
    plt.title('randomized linear')
    plt.show()

def plot_class_data(n_points, penetration):
    xy = numpy.random.rand(2, n_points)
    x = xy[0]
    y = xy[1]
    idx = 2 * x + 0.1 > y + penetration * numpy.random.randn(*x.shape)
    inv_idx = numpy.logical_not(idx)
    x1 = x[idx]
    y1 = y[idx]
    x2 = x[inv_idx]
    y2 = y[inv_idx]
    plt.subplot(1, 1, 1)
    plt.plot(x1, y1, 'g.')
    plt.plot(x2, y2, 'r.')
    plt.title('data')
    plt.show()

def plot_sine():
    x = numpy.arange(0, 3 * numpy.pi, 0.1)
    y = numpy.sin(x)
    plt.subplot(1, 1, 1)
    plt.plot(x, y)
    plt.title('sine')
    plt.show()

def load_save():
    x = numpy.arange(0, 3 * numpy.pi, 0.1)
    numpy.save('x_array', x)
    x_loaded = numpy.load('x_array.npy')
    print(x)
    print(x_loaded)

def axes():
    r = numpy.arange(2 * 3 * 4).reshape(2, 3, 4)
    r_t = numpy.transpose(r, (2, 1, 0))
    print(r_t)
    print('--------')
    print(r)
    print('-------- axis 0')
    print(numpy.sum(r, axis=0))
    print('-------- axis 1')
    print(numpy.sum(r, axis=1))
    print('-------- axis 2')
    print(numpy.sum(r, axis=2))

plot_class_data(2000, 0.1)
