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


#print ' a = ' , a[  :  ]             # a =  [1.0, 0.2, 0.3333333333333333, 5.0]
#print ' a = ' , a[ 0 : m ]           # a =  [1.0, 0.2, 0.3333333333333333]
print ' a = ' , a[ 2 : m ]            # a =  [0.3333333333333333]


k[ 2:4 ]  = kostas( a[1 : m] )       # k =  [0.0, 0.0, 0.4, 0.6666666666666666]
#k = kostas( a[0 : m+1] )            # k =  [2.0, 0.4, 0.6666666666666666, 10.0]
#k = kostas( a[:] )                  # k =  [2.0, 0.4, 0.6666666666666666, 10.0]

print ' k = ', k 
