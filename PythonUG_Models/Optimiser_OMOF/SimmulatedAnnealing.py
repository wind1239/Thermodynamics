
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math 
import random 
import time

import ReadOptList
import ThermoTools as ThT
import SATools as SAT


###
### Standard Simmulated Annealing Algorithm (SAA) and related functions
###


###
### FUNCTION: Global Variables
###

def set_Global_Variables2(): # Global variables
# Building up a small data bank for species C1 and C2:

###
### Random number generators 
###
def RandomNumberGenerator( n ):

    rn = []
    # Initialisation:
    for i in range( n ):
        r = random.SystemRandom()
        seed = time.time() # seed
        if ( seed % 2 > 1. ):
            r = random.SystemRandom( seed )
        else:
            r0, r1 = math.modf( seed )
            if ( i % 2 == 0 ):
                r = random.SystemRandom( r0 )
            else:
                r = random.SystemRandom( r1 )

        rn.append( r.random() )
                
    return rn


###
### Dealing with envelope-constraints
###
def Envelope_Constraints( n, X ):

    rand = []
    TryAgain = True
    while TryAgain:
        for i in range( n - 1 ):
            if ( ( X[ i ] < SAT.Lower[ i ] ) | ( X[ i ] > SAT.Upper[ i ] ) ):
                rand = RandomNumberGenerator( n )
                X[ i ] = SAT.Lower[ i ] + ( SAT.Upper[ i ] - SAT.Lower[ i ] ) * \
                    rand[ i ]
                X[ i ] = min( max( SAT.Lower[ i ], X[ i ] ), SAT.Upper[ i ] )

        Sum = ListSum( X[ 0 : n-1 ] )

        if ( Sum < 1. ):
            X[ n - 1 ] = 1. - Sum
            TryAgain == False
            return X
        else:
            for i in range( n ):
                rand = RandomNumberGenerator( n )
                X[ i ] = SAT.Lower[ i ] + ( SAT.Upper[ i ] - SAT.Lower[ i ] ) * \
                    rand[ i ]

    
###
### Summation of a list of reals
###
def ListSum( XList ):
    if ( len( XList ) == 1 ):
        return XList[ 0 ]
    else:
        return XList[ 0 ] + ListSum( XList[ 1 : ] )
    
