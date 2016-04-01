#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import calculate_fi_test as fi



def Calc_ChemPot( iphase, MFrac ):
    print ' i am not here '

    ChemPot = [0. for i in range(ThT.NComp) ] 
    lnPXi = [0. for i in range(ThT.NComp) ] 

    lnphi = fi.CALC_FI( iphase, MFrac ) # lnphi is the 1st term of the rhs of the equation and contains Nc components 

    # Calculating second-term of the rhs
    for icomp in range( ThT.NComp ):
         lnPXi[ icomp ] = math.log( ThT.P_System[ 0 ] * MFrac[ icomp ] )
         ChemPot[ icomp ] = ThT.Rconst * ThT.T_System[0] * ( lnphi[ icomp ] - lnPXi[ icomp ] )
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

'''
ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp ) ]
print '  the initial molar fraction before reading from the input.dat is ', MFrac
# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase

iphase = 0

phi = Calc_ChemPot( iphase, MFrac )
print 'ln_phi = ', phi 
'''

