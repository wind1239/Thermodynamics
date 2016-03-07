
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS as EoS


'''
    THIS FILE WAS DESIGNED TO CALCULATE MIXING AND COMBINING RULES FOR EQUATIONS
         OF STATE.
'''

# Given component, pressure and temperature, this function will return
#    the mixture attractive and repulsive parameters:
def MixingRules_EoS( Temp, Composition ):
    if ThT.MixingRules == "Classic":
        ( am, bm ) = MixingRules_EoS_Classic( Temp, Composition )
    elif ThT.MixingRules == "Wong-Sandler":
        ( am, bm ) = MixingRules_EoS_WongSandler( Temp, Composition )
    else:
        sys.exit( 'Mixing rules were not defined correctly!' )

    return am, bm
 

'''
 This function calculates the attraction and repulsion parameters for the classic mixing rule
      (quadratic and linear formulation for attraction and repulsion, respectively)
'''
def MixingRules_EoS_Classic( Temp, Composition ):
    # aij: parameter for the combining rule (stored in the same way as kij)
    aij = [ 0. for i in range( ThT.NComp**2 ) ] ; sum1 = 0. ; sum2 = 0.
    deriv = [ 0. for i in range( 2 ) ]
    
    for icomp in range( ThT.NComp ):
        ai, bi = Cubic_EoS( icomp, Temp )
        sum2 = sum2 + bi * Composition[ icomp ]
        
        for jcomp in range( ThT.NComp ):
            node = icomp * ThT.NComp + jcomp
            aj, bj = Cubic_EoS( jcomp, Temp )

            if icomp == jcomp:
                aij[ node ] = ai
            else:
                aij[ node ] = math.sqrt( ai * aj ) * ( 1. - ThT.kij[ node ] )

            sum1 = sum1 + aij[ node ] * Composition[ icomp ] * Composition[ jcomp ]

    am = sum1 ; bm = sum2 # am and bm (mixing rules)        

    return am, bm, deriv

'''
 This function calculates the attraction and repulsion parameters for the
       Wong-Sandler mixing rule
'''
def MixingRules_EoS_WongSandler( Temp, Composition ):

    sys.exit( 'Wong-Sandler mixing rule is not ready yet.' )

    return 
