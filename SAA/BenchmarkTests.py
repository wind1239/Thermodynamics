
#!/usr/bin/env python

import math 
import sys
import BenchmarkTests as BTest
import SAA_Tools as SaT
import SA_IO as IO

####
####
####  Jamil etal. (2013) Int. J. Math. Mod. and Num. Optimisation
####          4(2):150-194
####       (doi: 10.1504/IJMMNO.2013.055204)
####
####
def TestFunction( TestName, n, X ):
    """ Calling test functions """

    if ( TestName == 'Judge_Function' ):
        Result = TestFunction_Judge( n, X )

    elif ( TestName == 'Beale_Function' ):
        Result = TestFunction_Beale( n, X )
        
    elif ( TestName == 'Easom_Function' ):
        Result = TestFunction_Easom( n, X )

    elif ( TestName == 'CosineMixture_Function' ):
        Result = TestFunction_CosineMixture2D( n, X )

    elif( TestName == 'Booth_Function' ):
        Result = TestFunction_Booth( n, X )

    elif( TestName == 'Hosaki_Function' ):
        Result = TestFunction_Hosaki( n, X )

    elif ( TestName == 'Colville4D_Function' ):
        Result = TestFunction_Colville4D( n, X )

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

    if abs( FSA_Solution - F_Solution ) >= SaT.EPS :
        Pass = False

    IO.f_SAOutput.write( 'Assessment of function: {a:}. Analytical: {b:.4e}, Numerical: {c:.4e}. ==> Pass: {d:}'.format( a = SaT.Function_Name, b = F_Solution, c = FSA_Solution, d = Pass ) + '\n' )

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
def TestFunction_Colville4D( n, X ):
    """ Colville Function """

    F1 = 100. * ( X[ 0 ] - X[ 1 ]**2 )**2 + ( 1. - X[ 0 ] )**2 
    F2 = 90.  * ( X[ 3 ] - X[ 2 ]**2 )**2 + ( 1. - X[ 2 ] )**2
    F3 = 10.1 * ( ( X[ 1 ] - 1. ) ** 2 + ( X[ 3 ] - 1. )**2 )
    F4 = 19.8 * ( X[ 1 ] - 1. ) * ( X[ 3 ] - 1. )

    F = F1 + F2 + F3 + F4

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
         
####
####
####
def TestFunction_Judge( nd, theta ):
    """ Judge et al., The Theory and Practice of Econometrics, 2nd ed., pp. 956-7.
           There are two optima: F(.864,1.23) = 16.0817 (the global minumum) and
           F(2.35,-.319) = 20.9805.                                              """

    N = 20
    X2 = [ 0. for i in range( N ) ] ;  X3 = [ 0. for i in range( N ) ] ; \
         Y = [ 0. for i in range( N ) ]


    Y[0] = 4.284 ; Y[1] = 4.149 ; Y[2] = 3.877 ; Y[3] = 0.533 ; Y[4] = 2.211 ; Y[5] = 2.389 ; \
           Y[6] = 2.145 ; Y[7] = 3.231 ; Y[8] = 1.998 ; Y[9] = 1.379 ; Y[10] = 2.106 ; Y[11] = 1.428 ; \
                  Y[12] = 1.011 ; Y[13] = 2.179 ; Y[14] = 2.858 ; Y[15] = 1.388 ; Y[16] = 1.651 ; \
                          Y[17] = 1.593 ; Y[18] = 1.046 ; Y[19] = 2.152
    
    X2[0] = .286 ; X2[1] = .973 ; X2[2] = .384 ; X2[3] = .276 ; X2[4] = .973 ; X2[5] = .543 ; \
            X2[6] = .957 ; X2[7] = .948 ; X2[8] = .543 ; X2[9] = .797 ; X2[10] = .936 ; \
                    X2[11] = .889 ; X2[12] = .006 ; X2[13] = .828 ; X2[14] = .399 ; X2[15] = .617 ; \
                             X2[16] = .939 ; X2[17] = .784 ; X2[18] = .072 ; X2[19] = .889

    X3[0] = .645 ; X3[1] = .585 ; X3[2] = .310 ; X3[3] = .058 ; X3[4] = .455 ; X3[5] = .779 ; X3[6] = .259 ; \
            X3[7] = .202 ; X3[8] = .028 ; X3[9] = .099 ; X3[10] = .142 ; X3[11] = .296 ; X3[12] = .175 ; \
                    X3[13] = .180 ; X3[14] = .842 ; X3[15] = .039 ; X3[16] = .103 ; X3[17] = .620 ; \
                             X3[18] = .158 ; X3[19] = .704

    F = 0.
    for i in range( N ):
        F = F + ( theta[ 0 ] + theta[ 1 ] * X2[ i ] + ( theta[ 1 ]**2 ) * X3[ i ] - Y[ i ] )**2

    return F

