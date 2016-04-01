#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import calculate_fi_test as fi



def Calc_ChemPot( iphase, MFrac ):

    ChemPot = [0. for i in range(ThT.NComp) ] 
    lnPXi = [0. for i in range(ThT.NComp) ] 

    lnphi = fi.CALC_FI( iphase, MFrac ) # lnphi is the 1st term of the rhs of the equation and contains Nc components 

    # Calculating second-term of the rhs
    for icomp in range( ThT.NComp ):
         lnPXi[ icomp ] = math.log( ThT.P_System[ 0 ] * MFrac[ icomp ] )
         ChemPot[ icomp ] = ThT.RConst * ThT.T_System[0] * ( lnphi[ icomp ] - lnPxi[ icomp ] )
         print ' the chempot = ', ChemPot[ icomp ]
    return ChemPot

'''
def Calc_ChemPot( iphase, MFrac ):
    sz = np.shape(MFrac)[0]
    print ' now i am in the chempot function ' 
    chempot = ThT.Rconst * ThT.T_System[ 0 ] #* ( np.log( ThT.P_System[ 0 ] * MFrac[ i ] ) )
    print ' the chempot = ', chempot 
    
    return chempot
'''