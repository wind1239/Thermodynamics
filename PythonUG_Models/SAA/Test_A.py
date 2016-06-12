
#!/usr/bin/env python

import math 
import sys


def test1( N, X ):
    ### This template function is the Ackley 2 Function
    ###   
    ###     F(X1,X2) = -200 * exp( -0.02 * sqrt(X1**2 + X2**2))
    ###
    ### obtained from:
    ###  Jamil etal. (2013) Int. J. Math. Mod. and Num. Optimisation
    ###          4(2):150-194
    ###       (doi: 10.1504/IJMMNO.2013.055204)
    sum = 0.
    for i in range( N ):
        sum = sum + X[i]**2

    sum = math.sqrt( sum )
    F = -200. * math.exp( -.02 * sum )

    return F
    
