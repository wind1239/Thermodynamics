
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions
import ThermoTools as ThT
import EOS_PR as PR


###
### FUNCTION: Calculation of attraction and repulsion parameters for a simple mixing rule (quadratic and linear formulation for
###           attraction and repulsion, respectively)
###
def MixingRules(T, Composition ):
    ThT.set_Global_Variables()

    aij = [0. for i in range( ThT.Nc**2 ) ] # Parameter for the combining rule (stored in the same way as kij)

    sum1 = 0. ; sum2 = 0.
    for i in range( ThT.Nc ):
        for j in range( ThT.Nc ):
            node = i * ThT.Nc + j 
            if i == j:
                aij[ node ] = PR.PREoS_Calc_a( i , T )
            else:
                aij[ node ] = math.sqrt( PR.PREoS_Calc_a( i , T ) * PR.PREoS_Calc_a( j , T ) ) * ( 1. - ThT.kij[ node ] )

            sum1 = sum1 + aij[ node ] * Composition[ i ] * Composition[ j ]
        sum2 = sum2 + PR.PREoS_Calc_b( i ) * Composition[ i ]

    am = sum1 ; bm = sum2 # am and bm (mixing rules)        

    return am, bm
