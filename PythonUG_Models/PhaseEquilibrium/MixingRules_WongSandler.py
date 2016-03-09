
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS as EoS
import ActivityCoeffModels as ActMod


"""
 This function calculates the attraction and repulsion parameters for the
       Wong-Sandler mixing rule
"""
def MixingRules_EoS_WongSandler( Temp, Press, iphase, Composition ):

    """ Q Parameter:
            Q = SUMi[ SUMj ( Xi * Xj * (b-a/R*T)ij ) ]
        Derivative of Q wrt ni:
            [ 1/n d(n^2 Q)/dni ] = 2 SUMj [ Xj * (b-a/RT)ij ]  """
    ( QPar, d_QPar ) = Calc_QPar( Temp, Composition )

    """ D Parameter:
            D = SUMi[ Xi * ai/(bi*R*T) ] + AE/(C*R*T)
        Derivative of D wrt ni:
            [ d(nD)/dni ] = ai/(bi(R*T) + ln(Gammai) / C   """
    ( DPar, d_DPar ) = Calc_DPar( Temp, Composition )

    """ Calculating bm and am:
            bm = Q/(1-D)    &     am = bm*R*T*D """
    bm = QPar / ( 1. - DPar ) ; am = bm * DPar * ThT.RConst * Temp


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

    Big_A = am * Press / ( ThT.RConst * Temp )**2
    Big_B = bm * Press / ( ThT.RConst * Temp )
    
    """ Calculating the derivatives of am and bm wrt ni:
            [ d(nbm)/dni ] = 1/(1-D) * [ 1/n d(n^2 Q)/dni ] - 
                             Q/(1-D)**2 * ( 1 - [ d(nQ)/dni ] )
            1/(RT) * [ 1/n d(n^2 am)/dni ] = D * [ d(nbm)/dni ] +
                             bm * [ d(nD)/dni ]                  """
    
    d_am = [ 0. for i in range( ThT.NComp ) ]
    d_bm = [ 0. for i in range( ThT.NComp ) ]
    FugCoeff = [ 0. for i in range( ThT.NComp ) ]
    ChemPot = [ 0. for i in range( ThT.NComp ) ]

    for i in range( ThT.NComp ):
        d_bm[ i ] = ( 1. / ( 1. - DPar ) * d_QPar[ i ] ) - \
                    ( QPar / ( 1. - DPar )**2 * ( 1. - d_DPar[ i ] ) )
        
        d_am[ i ] = ThT.RConst * Temp * \
                    ( DPar * d_bm[ i ] + bm * d_DPar[ i ] )

        LogPhi = -math.log( max( 1.e-5, Z - Big_B ) ) + 1. / bm * d_bm[ i ] * ( Z - 1 ) + \
                 1. / ( 2. * math.sqrt( 2. ) ) * am / ( ThT.RConst * Temp * bm ) * \
                 ( 1. / am * d_am[ i ] - 1. / bm * d_bm[ i ] ) * \
                 math.log( max( 1.e-5, ( Z / Big_B + 1. - math.sqrt( 2. ) ) / \
                                ( Z / Big_B + 1. + math.sqrt( 2. ) ) ) )
        print 'Log', LogPhi
        FugCoeff[ i ] = math.exp( LogPhi )

        ChemPot[ i ] = ThT.RConst * Temp * LogPhi + math.log( Composition[ i ] * Press )
               
    return FugCoeff, ChemPot


#=================
#
#=================
def SecondVirialCoeff( icomp, jcomp, Temp ):
    """ This function calculates the second Virial coefficient expressed
        without dependency on the composition.
           (b - a/RT)ij =
               0.5 * [ (b - a/RT)i + (b - a/RT)j ] * ( 1 - Kij )
                                                                 """
    ai, bi = EoS.Cubic_EoS( icomp, Temp )
    aj, bj = EoS.Cubic_EoS( jcomp, Temp )

    node = icomp * ThT.NComp + jcomp

    abRT = 0.5 * ( ( bi - ai / ( ThT.RConst * Temp ) ) + ( bi - ai / ( ThT.RConst * Temp ) ) ) * \
           ( 1. - ThT.BinaryParameter[ node ] )

    return abRT


#=================
#
#=================
def Calc_QPar( Temp, X ):
    d_QPar = [ 0. for i in range( ThT.NComp ) ] ; QPar = 0.

    # Calculating QPar and d_QPar
    for i in range( ThT.NComp ):
        sum1 = 0. ; sum2 = 0.
        for j in range( ThT.NComp ):
            abRT = SecondVirialCoeff( i, j, Temp )
            sum1 = sum1 + X[ i ] * X[ j ] * abRT # for QPar
            sum2 = sum2 + X[ j ] * abRT          # for d_QPar
        QPar = QPar + sum1
        d_QPar[ i ] = d_QPar[ i ] + sum2

    return QPar, d_QPar

#=================
#
#=================
def Calc_DPar( Temp, X ):
    d_DPar = [ 0. for i in range( ThT.NComp ) ] ; DPar = 0

    """  C Parameter:
            C = ln[sqrt(2.)-1]/sqrt(2.) """
    C_Par = math.log( math.sqrt(2.) - 1. ) / math.sqrt(2.)


    """ Calculating Gibbs free energy in excess (which is
        the same as the Helmholtz free energy in excess
        (Ae/RT = Ge/RT) for the mixture and the activity
        coefficient (Gamma) for each component.            """
    AeRT, Gamma = ActMod.Activity_WilsonModel( X )

    sum1 = 0.
    for i in range( ThT.NComp ):
        ai, bi = EoS.Cubic_EoS( i, Temp )
        d_DPar[ i ] = ai / ( bi * ThT.RConst * Temp ) + math.log( max( 1.e-5, Gamma[ i ] ) )/ C_Par
        sum1 = sum1 + X[ i ] * ai / ( bi * ThT.RConst * Temp ) # for DPar

    DPar = sum1 + AeRT / C_Par

    return DPar, d_DPar
