
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math 
import sys
import AdaptiveSimulatedAnnealing as ASA


def TestFunction( TestName, n, X ):
    """ Calling test functions """

    if ( TestName == 'Dummy1' ):
        Result = TestFunction_Dummy2( n, X )
        
    elif ( TestName == 'Beale Function' ):
        Result = TestFunction_Beale( n, X )
        
    elif ( TestName == 'Easom Function' ):
        Result = TestFunction_Easom( n, X )

    elif ( TestName == 'Cosine Mixture Function' ):
        Result = TestFunction_CosineMixture2D( n, X )

    else:
        print '====> ', TestName, ' <===='
        sys.exit( 'Test function not found' )

    return Result


def TestFunction_Beale( n, X ):
    """ Beale function, Fmin @ (3., 0.5) """
    F1 = ( 1.5 - X[ 0 ] + X[ 0 ] * X[ 1 ] )**2
    F2 = ( 2.25 - X[ 0 ] + X[ 0 ] * X[ 1 ]**2 )**2
    F3 = ( 2.625 -X[ 0 ] + X[ 0 ] * X[ 1 ]**3 )**2
    F = F1 + F2 + F3

    return F

def TestFunction_Easom( n, X ):
    """ Easom Function """
    
    Pi = 4. * math.atan( 1. )
    F = - math.cos( X[ 0 ] ) * math.cos( X[ 1 ] ) * \
        math.exp( -( X[ 0 ] - Pi )**2 - ( X[ 1 ] - Pi )**2 )    

    return F

def TestFunction_CosineMixture2D( n, X ):
    """ Cosine Mixture (2D) Function """

    Pi = 4. * math.atan( 1. )
    F1 = 0.; F2 = 0.
    for i in range( n ):
        F1 = F1 + 1.e-1 * math.cos( 5. * Pi * X[ i ] )
        F2 = F2 + X[ i ]**2
        F = -( F1 + F2 )

    return F
                       

def TestFunction_Dummy2( n, X ):
    F = X[ 0 ]**2 - X[ 1 ]**2 + X[ 2 ]**3

    return F
