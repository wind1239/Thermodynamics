
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions
import ThermoTools
import EOS_PR


###
### FUNCTION: Calculation of attraction and repulsion parameters for a simple mixing rule (quadratic and linear formulation for
###           attraction and repulsion, respectively)
###
def MixingRules(T):
    set_Global_Variables()

    aij = [0. for i in range( Nc**2 ) ] # Parameter for the combining rule (stored in the same way as kij)

    sum1 = 0. ; sum2 = 0.
    for i in range( Nc ):
        for j in range( Nc ):
            node = i * Nc + j 
            if i == j:
                aij[ node ] = PREoS_Calc_a( i , T )
            else:
                aij[ node ] = math.sqrt( PREoS_Calc_a( i , T ) * PREoS_Calc_a( j , T ) ) * ( 1. - kij[ node ] )

            sum1 = sum1 + aij[ node ] * FeedMoleFraction[ i ] * FeedMoleFraction[ j ]
        sum2 = sum2 + PREoS_Calc_b( i ) * FeedMoleFraction[ i ]
    am = sum1 ; bm = sum2 # am and bm (mixing rules)

    return am, bm
