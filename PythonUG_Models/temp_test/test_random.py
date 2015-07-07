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

def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)

def to_bool(value):
    """
       Converts 'something' to boolean. Raises exception for invalid formats
           Possible True  values: 1, True, "1", "TRue", "yes", "y", "t"
           Possible False values: 0, False, None, [], {}, "", "0", "faLse", "no", "n", "f", 0.0, ...
    """
    if str(value).lower() in ("yes", "y", "true",  "t", "1"): return True
    if str(value).lower() in ("no",  "n", "false", "f", "0", "0.0", "", "none", "[]", "{}"): return False
    raise Exception('Invalid value for boolean conversion: ' + str(value))

xx = []
xx_opt = []
SA_Cooling = []
SA_Cooling_list = []
maxminoption = True
icount = 0


with open( 'sa.in', 'r' ) as f:
    for line in f:
        print 'icount:', icount
        if line[ 0 ] == '#':
            line.rstrip()
            SA_Cooling_list.append( line[ 2 : len( line ) - 1 ] )
            icount += 1
        elif ( line == '\n' ):
            line.rstrip()
        else:
            inner_list = []
            if ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Maxminoption' ) ):
                inner_list = [ elt.strip() for elt in line.split(',') ]
                SA_Cooling.append( to_bool( inner_list[0] ) )
            else:
                inner_list = [ float(elt.strip()) for elt in line.split(',') ]
                SA_Cooling.append( inner_list )

f.close()

SA_MinMax = SA_Cooling[ 0 ]           
SA_N = num( SA_Cooling[ 1 ][0] )
SA_Ns = num( SA_Cooling[ 2 ][0] )

print 'SA_MinMax, SA_N, SA_Ns:', SA_MinMax, SA_N, SA_Ns

xx = RandomNumberGenerator( SA_N )
xx_opt = xx


Envelope_Constraints( SA_N, xx )
Envelope_Constraints( SA_N, xx_opt )

print 'xx2:', xx, 'Sum2:', ListSum( xx )




""" Starting the SA """


xx_opt = xx
