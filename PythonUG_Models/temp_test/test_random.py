
#!/usr/bin/env python

import random
import time
import math
import sys
import numpy as np
import BenchmarkTests as BTest


    
###
### Summation of a list of reals
###
def ListSum( XList ):
    if ( len( XList ) == 1 ):
        return XList[ 0 ]
    else:
        return XList[ 0 ] + ListSum( XList[ 1 : ] )
    

def ReadAll_SA():
    """Reading SA cooling schedule."""
    global SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
        SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Testing, SA_Benchmarks

    SA_Cooling = []
    SA_Cooling_list = []
    
    SA_Benchmarks = []
    
    icount = 0
    ntest = 0
    SA_Testing = False


    with open( 'sa.in', 'r' ) as f:
        
        for line in f:
            if line[ 0 ] == '#':
                line.rstrip()
                SA_Cooling_list.append( line[ 2 : len( line ) - 1 ] )
                icount += 1
            elif ( line == '\n' ):
                line.rstrip()
            else:
                inner_list = []
                if ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Minimum' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]                    
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )
                    
                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Testing' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )
                    
                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ][0:9] ) == 'Benchmark' ) ):
                    ntest += 1
                    inner_list = [ elt.strip() for elt in line.split(',') ]
                    SA_Cooling.append( inner_list )
                    
                else:
                    inner_list = [ float(elt.strip()) for elt in line.split(',') ]
                    SA_Cooling.append( inner_list )

    f.close()

    SA_Minimum = SA_Cooling[ 0 ]           
    SA_N = num( SA_Cooling[ 1 ][0] )
    SA_NS = num( SA_Cooling[ 2 ][0] )
    SA_NT = num( SA_Cooling[ 3 ][0] )
    SA_MaxEvl = SA_Cooling[ 4 ][0]
    SA_EPS = SA_Cooling[ 5 ][0]
    SA_RT = SA_Cooling[ 6 ][0]
    SA_Temp = SA_Cooling[ 7 ][0] 

    SA_LowerBounds = SA_Cooling[ 8 ]
    SA_UpperBounds = SA_Cooling[ 9 ]
    SA_VM = SA_Cooling[ 10 ]
    SA_C = SA_Cooling[ 11 ]

    if SA_Cooling[ 12 ]:
        SA_Testing = SA_Cooling[ 12 ]

        if SA_Testing:
            if ( ntest > 1 ):
                for itest in range( ntest ):
                    jtest = len( SA_Cooling[ 12 + itest + 1 ] )
                    SA_Benchmarks.append( SA_Cooling[ 12 + itest + 1 ][ 0 : jtest ] )
            else:
                SA_Benchmarks = SA_Cooling[ 12 + ntest ][0]

            print 'SACooling:', len( SA_Benchmarks ), SA_Benchmarks[0][1]


    if ( SA_N != len( SA_LowerBounds ) ) or ( SA_N != len( SA_LowerBounds ) ) or \
            ( SA_N != len( SA_VM ) ) or ( SA_N != len( SA_C ) ):
               sys.exit("**** Stop !!! Dimensions do not match")  


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


def Envelope_Constraints( X, **kwargs ):
    """ This function ensures that variable 'X' is bounded """

    rand = []
    TryAgain = True

    if kwargs:
        for key in kwargs:
            if ( key == 'LBounds' ):
                LowerBounds = kwargs[ key ]
            elif ( key == 'UBounds' ):
                UpperBounds = kwargs[ key ]
            elif ( key == 'NDim' ):
                n = kwargs[ key ]
    else:
         LowerBounds = SA_LowerBounds
         UpperBounds = SA_UpperBounds
         n = SA_N
    



    print 'Ub:', UpperBounds
    print 'Lb:', LowerBounds, n

    
    evaluations = 1
    while TryAgain:
        for i in range( n - 1 ):
            #print 'loop1 ==>', i
            if ( (X[ i ] < LowerBounds[ i ]) | (X[ i ] > UpperBounds[ i ] )):
                rand = RandomNumberGenerator( n )
                X[ i ] = LowerBounds[ i ] + ( UpperBounds[ i ] - LowerBounds[ i ] ) * \
                    rand[ i ]
                X[ i ] = min( max( LowerBounds[ i ], X[ i ] ), UpperBounds[ i ] )

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

            
def num(s):
    """ Convert 'something' to either float or integer """
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



""" =========================================================================

           Main function: Starting the Simulated Annealing Algorithm

    =========================================================================  """

def SimulatedAnnealing()

    """ Reading the Cooling Schedule from the sa.in file """
    ReadAll_SA()

    """ Initialising a few key variables """
    xx = [] # xx will be obtained from the up routine
    xx_opt = []
    func = []
    func_opt = []
    fstar = []

    """ Initilising and bounding the X variable """
    if SA_Testing:
        for itest in range( len( SA_Benchmarks ) ):

            Dimension = int( SA_Benchmarks[ itest ][ 1 ] )
            """ This need to be modified to be obtained from the function calling """
            xx = RandomNumberGenerator( Dimension )
            i = 2 + Dimension
            j = i + Dimension
            Lower_Bounds = ( SA_Benchmarks[ itest ][ 2 : i  ] )
            Lower_Bounds = [ float( res ) for res in Lower_Bounds]
            Upper_Bounds = ( SA_Benchmarks[ itest ][ i : j  ] )
            Upper_Bounds = [ float( res ) for res in Upper_Bounds]

            Envelope_Constraints( xx , NDim = Dimension, LBounds = Lower_Bounds, UBounds = Upper_Bounds )

    else:
        xx = RandomNumberGenerator( SA_N )
        Envelope_Constraints( xx )

    xx_opt = xx
    print 'xx:', xx, SA_Testing

    """ Checking if the initial temperature is negative """
    if ( SA_Temp <= 0. ):
        sys.exit("*** Stop! Negative SA temperature")

    """ Calling the objective function for the first time """

    if SA_Testing:
        for itest in range( len( SA_Benchmarks ) ):
            TestName =  SA_Benchmarks[ itest ][ 0 ]
            Dimension = int( SA_Benchmarks[ itest ][ 1 ] )
            func.append( BTest.TestFunction( TestName, Dimension, xx ) )

    else:
        TestName = 'Dummy1'
        func.append( BTest.TestFunction( TestName, SA_N, xx ) )

    print 'xx2:', xx, 'Sum2:', ListSum( xx ), 'with ', func


    """ The function must be minimum, thus, in order to avoid any 
          possible mess all the signals may be changed """
    if SA_Minimum:
        func = [ - res for res in func ]

    fstar.append( func )

    print 'func, fstar:', func, fstar

    if SA_Testing:
        for itest in range( len( SA_Benchmarks ) ):

            TestName =  SA_Benchmarks[ itest ][ 0 ]                
            Dimension = int( SA_Benchmarks[ itest ][ 1 ] )
            i = 2 + Dimension
            j = i + Dimension
            Lower_Bounds = ( SA_Benchmarks[ itest ][ 2 : i  ] )
            Lower_Bounds = [ float( res ) for res in Lower_Bounds]
            Upper_Bounds = ( SA_Benchmarks[ itest ][ i : j  ] )
            Upper_Bounds = [ float( res ) for res in Upper_Bounds]

            xxopt, fopt = AdaptiveSimulatedAneealing( TestName, Dimension, Lower_Bounds, Upper_Bounds )

    else:
        xxopt, fopt = AdaptiveSimulatedAneealing( TestName, SA_N, SA_LowerBounds, SA_UpperBounds )




    """ Beginning of the main outter loop: """
    while kloop <= SA_MaxEvl:
        #NUp, NDown, NRej, NNew, LNobds

        while m <= SA_NT:

            while j <= SA_NS:

                while h <= SA_N:



                """ End of j loop """
                j += 1


            """ End of m loop """
            m += 1



        """  End of kloop """
        kloop += 1

