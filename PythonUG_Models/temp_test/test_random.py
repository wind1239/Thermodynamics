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

    Upper = [ 0.99999 for i in range( n ) ]
    Lower = [ 0.00001 for i in range( n ) ]

    rand = []
    TryAgain = True
    evaluations = 1
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
            print 'number of evaluations:', evaluations
            return X
        else:
            for i in range( n - 1 ):
                rand = RandomNumberGenerator( n )
                if( evaluations % 3 == 0 ):
                    X[ i ] = rand[ i ]
                elif ( evaluations % 7 == 0 ):
                    X[ i ] = min( rand[ i ], rand[ i + 1 ] ) / max( 1.e-7, float( i ), 1. / rand[ i + 1 ] )
                elif ( evaluations % 11 == 0 ):
                    X[ i ] = abs( 1. - rand[ i ] / rand[ i + 1 ] )
                else:
                    X[ i ] = rand[ i - 1 ]

            evaluations = evaluations + 1

            
def skip_comments( file ):
    for line in file:
        if not line.strip().startswith( '#' ):
            yield line


xx = []
xx_opt = []
SA_Cooling = []
maxminoption = True


with open( 'sa.in', 'rt' ) as f:
    for line in skip_comments( f ):
        #n = f.read( line )
        SA_Cooling.append( line )
        

#print 'n::', n
print 'SA', len( SA_Cooling ), SA_Cooling
        
xx = RandomNumberGenerator( n )
xx_opt = xx

Envelope_Constraints( n, xx )
Envelope_Constraints( n, xx_opt )

print 'xx2:', xx, 'Sum2:', ListSum( xx )




""" Starting the SA """


xx_opt = xx
