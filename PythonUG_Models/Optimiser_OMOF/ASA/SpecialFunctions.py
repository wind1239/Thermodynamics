
#!/usr/bin/env python

import math
import sys
import RandomGenerator as RanGen

   
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

    else:
         LowerBounds = SA_LowerBounds
         UpperBounds = SA_UpperBounds
         n = SA_N
    
    evaluations = 1
    
    if ( IsNormalised ): # Mole/Mass/Volume Fraction
        dim = n - 1
    else:
        dim = n

        
    while TryAgain:
           
        for i in range( dim ):
            if ( ( X[ i ] < LowerBounds[ i ]) | (X[ i ] > UpperBounds[ i ] ) ):
                rand = RanGen.RandomNumberGenerator( n, LowerBounds, UpperBounds )
                X[ i ] = LowerBounds[ i ] + ( UpperBounds[ i ] - LowerBounds[ i ] ) * \
                    rand[ i ]
                X[ i ] = min( max( LowerBounds[ i ], X[ i ] ), UpperBounds[ i ] )
                Try = True

        if ( IsNormalised ):
            
            Sum = ListSum( X[ 0 : dim ] )

            if ( Sum < 1. ):
                X[ n - 1 ] = 1. - Sum
                TryAgain = False
                if kwargs:
                    for key in kwargs:
                        if ( key == 'TryC' ):
                            return X, Try
                        else:
                            return X
                else:
                    return X
            else:
                for i in range( dim ):
                    rand = RanGen.RandomNumberGenerator( n, LowerBounds, UpperBounds )
                    if( evaluations % 3 == 0 ):
                        X[ i ] = rand[ i ]
                    elif ( evaluations % 7 == 0 ):
                        X[ i ] = min( rand[ i ], rand[ i + 1 ] ) / max( MinNum, float( i ), 1. / rand[ i + 1 ] )
                    elif ( evaluations % 11 == 0 ):
                        X[ i ] = abs( 1. - rand[ i ] / rand[ i + 1 ] )
                    else:
                        X[ i ] = rand[ i - 1 ]

                evaluations = evaluations + 1
                Try = True

        else:
            if kwargs:
                for key in kwargs:
                    if ( key == 'TryC' ):
                        return X, Try
                    else:
                        return X
            else:
                return X


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

