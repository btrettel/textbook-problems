#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy.random

def mean(x):
    return sum(x) / len(x)

def variance(x):
    return mean((x - mean(x))**2)

def stdev(x):
    return variance(x)**(1./2.)

def skewness(x):
    return mean(((x - mean(x)) / stdev(x))**3)

def kurtosis(x):
    return mean(((x - mean(x)) / stdev(x))**4)

N            = 10**5
random_array = numpy.random.rand(N, 1)

print(mean(random_array), 0.5)
print(variance(random_array), 1./12.)
print(stdev(random_array), numpy.sqrt(1./12.))
print(skewness(random_array), 0.)
print(kurtosis(random_array), 3. - 6./5.)