import random
import time
import math
import sys
import numpy as np


    
###
### Summation of a list of reals
###
def ListSum( XList ):
    if ( len( XList ) == 1 ):
        return XList[ 0 ]
    else:
        return XList[ 0 ] + ListSum( XList[ 1 : ] )
    


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


def Envelope_Constraints( n, X ):

    Upper = [ 1. for i in range( n ) ]
    Lower = [ 0. for i in range( n ) ]

    rand = []
    TryAgain = True
    #print 'n::', n
    while TryAgain:
        for i in range( n - 1 ):
            #print 'loop1 ==>', i
            if ( (X[ i ] < Lower[ i ]) | (X[ i ] > Upper[ i ] )):
                rand = RandomNumberGenerator( n )
                X[ i ] = Lower[ i ] + ( Upper[ i ] - Lower[ i ] ) * \
                    rand[ i ]
                X[ i ] = min( max( Lower[ i ], X[ i ] ), Upper[ i ] )

        Sum = ListSum( X[ 0 : n-1 ] )

        if ( Sum < 1. ):
            X[ n - 1 ] = 1. - Sum
            TryAgain == False
            print 'Sum:::', Sum
            return X
        else:
            for i in range( n ):
                rand = RandomNumberGenerator( n )
                X[ i ] = Lower[ i ] + ( Upper[ i ] - Lower[ i ] ) * \
                    rand[ i ]



xx = []
n = 3

xx = RandomNumberGenerator( n )
print 'xx:', xx, 'Sum:', ListSum( xx )

Envelope_Constraints( n, xx )

print 'xx2:', xx, 'Sum2:', ListSum( xx )
