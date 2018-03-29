
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS as EoS
import MixingRules_WongSandler as Mix_WS
#import ActivityCoeffModels as ActMod


'''
 This function calculates the attraction and repulsion parameters for the classic mixing rule
      (quadratic and linear formulation for attraction and repulsion, respectively)
'''
def MixingRules_EoS_Classic( Temp, Press, iphase, Composition ):
    
    # aij: parameter for the combining rule (stored in the same way as kij)
    aij = [ 0. for i in range( ThT.NComp**2 ) ] 
    FugCoeff = [ 0. for i in range( ThT.NComp ) ]
    ChemPot = [ 0. for i in range( ThT.NComp ) ]

    sum2 = 0.
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
    Big_A = am * Press / ( Temp * ThT.RConst )**2
    Big_B = bm * Press / ( Temp * ThT.RConst )
        
    """ Calculating the Compressibility Factor (Z) of phase iphase     """
    Zvapour, Zliquid = EoS.PR_Cubic( Temp, Press, am, bm )
    if iphase == 0: # Vapour phase
        Z = Zvapour
    elif iphase == 1: # Liquid phase
        Z = Zliquid
    else:
        sys.exit('At MixingRules_EoS_Classic function. Variable iphase not found')

    ###BigAA = [ 0. for i in range( ThT.NComp ) ] ; BigBB = [ 0. for i in range( ThT.NComp ) ]
    for icomp in range( ThT.NComp ):
        ai, bi = EoS.Cubic_EoS( icomp, Temp )
        sum3 = 0.
        for jcomp in range( ThT.NComp ):
            node = icomp * ThT.NComp + jcomp
            aj, bj = EoS.Cubic_EoS( jcomp, Temp )

            if icomp == jcomp:
                aij[ node ] = ai
            else:
                aij[ node ] = math.sqrt( ai * aj ) * ( 1. - ThT.BinaryParameter[ node ] )

            sum3 = sum3 + aij[ node ]

        BigAA = 2. / max( ThT.Residual, am) * sum3
        BigBB = bi / max( ThT.Residual, bm)

        LogPhi = BigBB * ( Z - 1 ) - math.log( max( ThT.Residual, Z - Big_B ) ) - Big_A / max( ThT.Residual, 2.*math.sqrt(2.) * Big_B ) * ( BigAA - BigBB ) * math.log( max( ThT.Residual, ( Z + Big_B * ( math.sqrt(2.) + 1. ) ) ) /  max( ThT.Residual, ( Z + Big_B * ( math.sqrt(2.) - 1. ) ) ) )

        FugCoeff[ icomp ] = math.exp( LogPhi )

        ChemPot[ icomp ] = ThT.RConst * Temp * LogPhi + math.log( max( ThT.Residual, Composition[ i ] * Press ) )

    return FugCoeff, ChemPot

        

    

    
    
