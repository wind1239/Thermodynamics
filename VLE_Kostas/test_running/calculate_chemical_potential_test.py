#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import calculate_fi_test as fi

def Calc_ChemPot( iphase, MFrac ):
    print ' * i am finally in the chemical potetnial function '

    ChemPot = [0. for i in range(ThT.NComp) ] 
    lnPXi = [0. for i in range(ThT.NComp) ]
    #print 
    #print ' ---------------------------------------------------------------------------- lnPXi =',lnPXi, ' ChemPot =', ChemPot 
    #print
    lnphi = fi.CALC_FI( iphase, MFrac ) # lnphi is the 1st term of the rhs of the equation and contains Nc components 
    #print ' --------------------------------------------------------------------- the lnphi from above is = ', lnphi 
    # Calculating second-term of the rhs
    for icomp in range( ThT.NComp ):
         lnPXi[ icomp ] = math.log( ThT.P_System[ 0 ] * MFrac[ icomp ] )
         ChemPot[ icomp ] = ThT.Rconst * ThT.T_System[0] * ( lnphi[ icomp ] - lnPXi[ icomp ] )
         #print '  I am in the chempot function --- the chempot = ', ChemPot[ icomp ]
    return ChemPot
