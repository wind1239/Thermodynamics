
#!/usr/bin/env python

import math 
import sys
import BenchmarkTests as BTest
import SAA_Tools as SaT

####
####
####  Jamil etal. (2013) Int. J. Math. Mod. and Num. Optimisation
####          4(2):150-194
####       (doi: 10.1504/IJMMNO.2013.055204)
####
####
def TestFunction( TestName, n, X ):
    """ Calling test functions """

    if ( TestName == 'Beale_Function' ):
        Result = TestFunction_Beale( n, X )
        
    elif ( TestName == 'Easom_Function' ):
        Result = TestFunction_Easom( n, X )

    elif ( TestName == 'CosineMixture_Function' ):
        Result = TestFunction_CosineMixture2D( n, X )

    elif( TestName == 'Booth_Function' ):
        Result = TestFunction_Booth( n, X )

    elif( TestName == 'Hosaki_Function' ):
        Result = TestFunction_Hosaki( n, X )

    else:
        print '====> ', TestName, ' <===='
        sys.exit( 'Test function not found' )

    return Result

####
####
####
def AssessTests( XSA_Solution, Solution ):
    """ This function assess solutions obtained from the SAA by comparing them
           (solution-coordinates and function value) against the known
           solution.                                                           """

    FSA_Solution = BTest.TestFunction( SaT.Function_Name, SaT.Ndim, XSA_Solution )
    F_Solution = BTest.TestFunction( SaT.Function_Name, SaT.Ndim, Solution[ 0 : SaT.Ndim ] )

    Pass = True 

    if abs( FSA_Solution - F_Solution ) >= 1.e-2 :
        Pass = False

    print 'Assessment of the function:', Pass, ' with computed and analytical solution of: ', FSA_Solution, F_Solution

    return Pass

####
####
####
def TestFunction_Beale( n, X ):
    """ Beale function, Fmin @ (3., 0.5) """
    F1 = ( 1.5 - X[ 0 ] + X[ 0 ] * X[ 1 ] )**2
    F2 = ( 2.25 - X[ 0 ] + X[ 0 ] * X[ 1 ]**2 )**2
    F3 = ( 2.625 -X[ 0 ] + X[ 0 ] * X[ 1 ]**3 )**2
    F = F1 + F2 + F3

    return F

####
####
####
def TestFunction_Easom( n, X ):
    """ Easom Function """
    
    Pi = 4. * math.atan( 1. )
    F = - math.cos( X[ 0 ] ) * math.cos( X[ 1 ] ) * math.exp( -( X[ 0 ] - Pi )**2 - ( X[ 1 ] - Pi )**2 ) 

    return F

####
####
####
def TestFunction_CosineMixture2D( n, X ):
    """ Cosine Mixture (2D) Function """

    Pi = 4. * math.atan( 1. )
    F = 0.; F1 = 0.; F2 = 0.
    for i in range( n ):
        F1 = F1 + math.cos( 5. * Pi * X[ i ] )
        F2 = F2 + X[ i ]**2

    F = -1.e-1 * F1 - F2 

    return F

####
####
####
def TestFunction_Booth( n, X ):
    """ Booth Function """
    F = ( X[0] + 2. * X[1] - 7. )**2 + ( 2. * X[0] + X[1] - 5 )**2

    return F
    
                       
####
####
####
def TestFunction_Hosaki( n, X ):
    F = ( 1. - 8. * X[0] + 7. * X[0]**2 - 7./3. * X[0]**3 + 1./4. * X[0]**4 ) * X[1]**2 * math.exp( -X[1] )

    return F
