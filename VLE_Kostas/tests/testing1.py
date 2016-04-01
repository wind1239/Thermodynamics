#!/usr/bin/env python
import numpy as np
import math
import sys

def kostas( b ):
    #print ' b = ', b
    kt = [0. for i in range( np.shape(b)[0] ) ]
    for i in range( np.shape(b)[0] ):
        kt[i] = b[i]*2.
    return kt




n = 0; m = 3

k = [0. for i in range( m+1 ) ]; print ' k init = ', k

a = [0. for i in range( m+1 ) ]
a[0] = 1.; a[1]=0.2; a[2]=1./3.; a[3]=5.

#for i in range( 2, m+1 ):
#    print i, a[i]


print ' a = ' , a[ 1 : m ]

k[ 2:4 ]  = kostas( a[1 : m] )
#k  = kostas( a[1 : m] ) 


print ' k = ', k 


