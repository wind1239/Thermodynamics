import numpy as np
import scipy as sc
import math
import sys

func = [ 2.e-1, 1.5, -3e-10, -547.02, 10.71 ]

f = np.sort(func); print ' f sorted = ', f
n = np.argsort(func); print ' indeces = ', n

for i in range( 5 ):
    print func[ n[i] ]

print ' the argsort prints the values from min --> max '
print ' the min = ', func[ n[0] ], ' max = ', func[ n[4] ] 
