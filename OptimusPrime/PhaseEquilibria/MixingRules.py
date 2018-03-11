
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS as EoS
import MixingRules_WongSandler as Mix_WS


'''
    THIS FILE WAS DESIGNED TO CALCULATE MIXING AND COMBINING RULES FOR EQUATIONS
         OF STATE.
'''

# Given component, pressure and temperature, this function will return
#    the mixture attractive and repulsive parameters:
def MixingRules_EoS( Temp, Press, iphase, Composition ):
    """ Composition has dimension NComp. """
    if ThT.MixingRules[ 0 ] == "Classic":
        ChemPot = [ 0. for i in range( ThT.NComp ) ]
        FugCoeff = [ 0. for i in range( ThT.NComp ) ]
        ( am, bm ) = MixingRules_EoS_Classic( Temp, Composition )
        
        """ This is temporary hack. This is valid only for VLE systems.
            Calculating the Compressibility Factor (Z)                  """
        Zvapour, Zliquid = EoS.PR_Cubic( Temp, Press, am, bm )
        if iphase == 0: # Vapour phase
            Z = Zvapour
        elif iphase == 1: # Liquid phase
            Z = Zliquid
        else:
            print 'Fix this hack !!'
            sys.exit()

        #print 'This needs to be fixed ...'
        #sys.exit()
            
    elif ThT.MixingRules[ 0 ] == "Wong-Sandler":
        ( FugCoeff, ChemPot ) =  Mix_WS.MixingRules_EoS_WongSandler( Temp, Press, iphase, Composition )
        
    else:
        sys.exit( 'Mixing rules were not defined correctly!' )

    return ( FugCoeff, ChemPot )
 

'''
 This function calculates the attraction and repulsion parameters for the classic mixing rule
      (quadratic and linear formulation for attraction and repulsion, respectively)
'''
def MixingRules_EoS_Classic( Temp, Composition ):
    # aij: parameter for the combining rule (stored in the same way as kij)
    aij = [ 0. for i in range( ThT.NComp**2 ) ] ; sum1 = 0. ; sum2 = 0.
    
    for icomp in range( ThT.NComp ):
        ai, bi = EoS.Cubic_EoS( icomp, Temp )
        sum2 = sum2 + bi * Composition[ icomp ]

        sum1 = 0.        
        for jcomp in range( ThT.NComp ):
            node = icomp * ThT.NComp + jcomp
            aj, bj = EoS.Cubic_EoS( jcomp, Temp )

            if icomp == jcomp:
                aij[ node ] = ai
            else:
                #aij[ node ] = math.sqrt( ai * aj ) * ( 1. - ThT.kij[ node ] )
                aij[ node ] = math.sqrt( ai * aj ) * ( 1. - ThT.BinaryParameter[ node ] )

            sum1 = sum1 + aij[ node ] * Composition[ icomp ] * Composition[ jcomp ]

    am = sum1 ; bm = sum2 # am and bm (mixing rules)
    print am, bm ; sys.exit('@@@')

    # need to calculate the and bm derivatives ... later

    return am, bm
