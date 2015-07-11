
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math 
import sys


def TestFunction( TestName, n, X ):
    """ Calling test functions """

    if ( TestName == 'Dummy1' ):
        Result = TestFunction_Dummy2( n, X )
        
    elif ( TestName == 'Beale Function' ):
        Result = TestFunction_Beale( n, X )
        
    elif ( TestName == 'Dummy Function' ):
        Result = TestFunction_Dummy( n, X )

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

def TestFunction_Dummy( n, X ):
    F = X[ 0 ]**2 - X[ 1 ]**2

    return F

def TestFunction_Dummy2( n, X ):
    F = X[ 0 ]**2 - X[ 1 ]**2 + X[ 2 ]**3

    return F
