
#!/usr/bin/env python

import math
import sys, time
import RandomGenerator as RanGen
import SAA_Tools as SaT
import SA_IO as IO
import ThermoTools as ThT
import pdb

   
###
### Summation of a list of reals
###
def ListSum( XList ):
    if ( len( XList ) == 1 ):
        return XList[ 0 ]
    else:
        return XList[ 0 ] + ListSum( XList[ 1 : ] )


###
### Imposing constraints into field variables
###
def Envelope_Constraints( X, **kwargs ):
    """ This function ensures that variable 'X' is bounded """

    rand = []
    TryAgain = True
    IsNormalised = True

    if kwargs:
        for key in kwargs:
            if ( key == 'LBounds' ):
                LowerBounds = kwargs[ key ]
            elif ( key == 'UBounds' ):
                UpperBounds = kwargs[ key ]
            elif ( key == 'NDim' ):
                n = kwargs[ key ]
            elif ( key == 'TryC' ):
                Try = kwargs[ key ]
            elif ( key == 'IsNormalised' ):
                IsNormalised = kwargs[ key ]
            elif ( key == 'Z_Feed' ):
                Z_Feed = kwargs[ key ]

    else:
         LowerBounds = SaT.LowerBounds
         UpperBounds = SaT.UpperBounds
         n = SaT.Ndim
    
    evaluations = 1
    
    if ( IsNormalised ): # Mole/Mass/Volume Fraction
        dim = n - 1
    else:
        dim = n
        
    while TryAgain:

        #pdb.set_trace()
           
        for i in range( n ):
            if ( ( X[ i ] < LowerBounds[ i ]) or ( X[ i ] > UpperBounds[ i ] ) ):
                rand = RanGen.RandomNumberGenerator( n )
                X[ i ] = LowerBounds[ i ] + ( UpperBounds[ i ] - LowerBounds[ i ] ) * \
                    rand[ i ]
                X[ i ] = min( max( LowerBounds[ i ], X[ i ] ), UpperBounds[ i ] )
                Try = True

        if ( IsNormalised ): # Thermod problem
            
            Sum = ListSum( X[ 0 : dim ] )
            print 'Sum:', Sum

            if ( Sum < 1. ):
                SumOneOtherPhase = CalcOtherPhase( X, UpperBounds, LowerBounds )
                if SumOneOtherPhase:
                    TryAgain = False
                    if kwargs:
                        for key in kwargs:
                            if ( key == 'TryC' ):
                                return X, Try
                            else:
                                return X
                    else:
                        return X
                else: # It did not satisfy the Box Formulation
                    rand = RanGen.RandomNumberGenerator( n )
                    for i in range( n ):
                        X[ i ] = LowerBounds[ i ] + ( UpperBounds[ i ] - LowerBounds[ i ] ) * \
                            rand[ i ]
                        X[ i ] = min( max( LowerBounds[ i ], X[ i ] ), UpperBounds[ i ] )
                    
            else:
                #for i in range( dim ): # This may need to be re-assesed later ...
                for i in range( n ): # This may need to be re-assesed later ...
                    rand = RanGen.RandomNumberGenerator( n )
                    if( evaluations % 3 == 0 ):
                        X[ i ] = rand[ i ]
                    elif ( evaluations % 7 == 0 ):
                        X[ i ] = min( rand[ i ], rand[ i + 1 ] ) / max( MinNum, float( i ), 1. / rand[ i + 1 ] )
                    elif ( evaluations % 11 == 0 ):
                        X[ i ] = abs( 1. - rand[ i ] / rand[ i + 1 ] )
                    else:
                        X[ i ] = rand[ i - 1 ]

                evaluations = evaluations + 1
                
                Sum = ListSum( X[ 0 : dim ] )
                if ( Sum < 1. ):            
                    SumOneOtherPhase = CalcOtherPhase( X, Z_Feed, UpperBounds, LowerBounds )
                    Try = True
                    if SumOneOtherPhase:
                        TryAgain = False
                        if kwargs:
                            for key in kwargs:
                                if ( key == 'TryC' ):
                                    return X, Try
                                else:
                                    return X
                        else:
                            return X

            """ ################################################################################
                  Here it needs to be add a function to ensure feasibility of the composition
                    solution-coordinates,
                                  XiL = ( Z - (1-L) *XiV ) / L
                    This means we need to add a function linking the
                    SAA and the thermodynamic functions.
                ################################################################################"""

        else: # Not a Thermod problem
            if kwargs:
                for key in kwargs:
                    if ( key == 'TryC' ):
                        return X, Try
                    else:
                        return X
            else:
                return X

###
### Calculating the other phase
###
def CalcOtherPhase( X, UB, LB, **kwargs ):
    """ This function calculates the composition of the other phase. The input is X[0:N-1], where X[N-1]
            is the phase composition. Z[0:N-1] is the feed composition. UB and LB are the lower and upper
            bound arrays.                                                                               """
    N = len( ThT.Z_Feed )
    Lphase = X[ N - 1 ] ;  Vphase = 1. - Lphase
    
    MFrac = [ 0. for i in range( N * N ) ] # Generating an null array
    for i in range( N - 1 ):
        MFrac[ i ] = X[ i ]

    MFrac[ N - 1 ] = 1. - ListSum( MFrac[ 0 : N - 1 ] )

    for i in range( N ):
        MFrac[ N + i ] = ( ThT.Z_Feed[ i ] - Lphase * MFrac[ i ] ) / Vphase

    if kwargs: # extra diagnostics ... print the components mol fraction and phase mol fraction
        for key in kwargs:
            if ( key == 'Diagnostics' ):
                Diag = kwargs[ key ]
                if Diag:
                    IO.f_SAOutput.write( '\n' )
                    IO.f_SAOutput.write( 'Composition (liq and vap): {a:}'.format( a = MFrac ) + '\n' )
                    IO.f_SAOutput.write( 'Liquid Phase Molar Fraction: {a:}'.format( a = Lphase ) + '\n' )
                    

    for i in range( N ): # Checking bounds at the other phase
        if MFrac[ N + i ] < LB[ i ] or MFrac[ N + i ] > UB[ i ]:
            TestOtherPhase = False

    SumOtherPhase =  ListSum( MFrac[ N:N*N ] )
    if abs( SumOtherPhase - 1. ) <= ThT.Residual:
        TestOtherPhase = True
    else:
        TestOtherPhase = False
    return TestOtherPhase
           

###
### Converting anything to wither integer or float
###            
def num(s):
    """ Convert 'something' to either float or integer """
    try:
        return int(s)
    except ValueError:
        return float(s)

###
### Converting string to boolean
###
def to_bool(value): 
    """
       Converts 'something' to boolean. Raises exception for invalid formats
           Possible True  values: 1, True, "1", "TRue", "yes", "y", "t"
           Possible False values: 0, False, None, [], {}, "", "0", "faLse", "no", "n", "f", 0.0, ...
    """
    if str(value).lower() in ("yes", "y", "true",  "t", "1"): return True
    if str(value).lower() in ("no",  "n", "false", "f", "0", "0.0", "", "none", "[]", "{}"): return False
    raise Exception('Invalid value for boolean conversion: ' + str(value))

###
###
###
def CreateDummyArray( n, X, F, eps ):
    for i in range( n ):
        X[ i ] = eps
    F = 1. / eps

    return X, F
