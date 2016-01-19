
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions
import ThermoTools as ThT
import EOS_PR as PR
import MixingRule_Simple as MixS



###
### FUNCTION: Calculate fugacity of components
###
def CalcFugacity( T, P, MoleFraction, phase ):
    
    ThT.set_Global_Variables()

    fugacity = [ 0. for i in range( ThT.Nc ) ]
    mixpar = [ 0. for i in range( ThT.Nc ) ]
    mixpar = MixS.MixingRules( T, MoleFraction ) # am and bm
    am = mixpar[ 0 ] ; bm = mixpar[ 1 ]

    Big_A = am * P / ( ThT.Rconst * T )**2
    Big_B = bm * P / ( ThT.Rconst * T )

    ( Zvapour, Zliquid ) = PR.Cubic_PR( T, P, am, bm )
    if phase == 'liquid':
        Z = Zliquid
    else:
        Z = Zvapour

    
    aij = [0. for i in range( ThT.Nc**2 ) ] # Parameter for the combining rule (stored in the same way as kij)
        
    for i in range( ThT.Nc ):
        sum1 = 0.
        for j in range( ThT.Nc ):
            node = i * ThT.Nc + j 
            if i == j:
                aij[ node ] = PR.PREoS_Calc_a( i , T )
            else:
                aij[ node ] = math.sqrt( PR.PREoS_Calc_a( i , T ) * PR.PREoS_Calc_a( j , T ) ) * ( 1. - ThT.kij[ node ] )
            sum1 = sum1 + aij[ node ] * MoleFraction[ j ]
            
        fugacity[ i ] = ( PR.PREoS_Calc_b( i ) / bm * ( Z - 1. ) - math.log( Z - Big_B ) - Big_A / ( 2. * math.sqrt( 2. ) * Big_B ) *
                       ( 2. * sum1 / am - PR.PREoS_Calc_b( i ) / bm  ) * math.log( ( Z + ( 1. - math.sqrt( 2. ) ) * Big_B ) /
                                                                                   ( Z + ( 1. + math.sqrt( 2. ) ) * Big_B ) )  )
        fugacity[ i ] = MoleFraction[ i ] * P * math.exp( fugacity[ i ] )

    return fugacity
        
