#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

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

def f(x):
    mu    = 2.
    sigma = 2.
    return ((2. * np.pi * sigma)**(1./2.)) * np.exp(-((x - mu)**2) / (2. * sigma**2))

N = 2*10**4 # > run the sampler for more than $10^4$ steps.

print("Running MCMC...")

x_prev = 0 # > Initialize the sampler with $x = 0$.
x_array = np.empty(0)
for i in range(N):
    x_prime = np.random.normal(x_prev, 1.) # Hogg step. 1
    r       = np.random.rand()             # Hogg step. 2
    
    if f(x_prime) / f(x_prev) > r: # Hogg step. 3
        x_prev = x_prime
        #print(x_prime, "keeping")
        x_array = np.append(x_array, x_prime)
    #else:
        #print(x_prime, "rejecting")

plt.hist(x_array)
plt.grid()
plt.savefig('p2.png')

print("mean:", mean(x_array))
print("stdev:", stdev(x_array))
print("skewness:", skewness(x_array))
print("kurtosis:", kurtosis(x_array))