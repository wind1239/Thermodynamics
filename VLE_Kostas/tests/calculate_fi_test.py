#!/usr/bin/env python

# a function that contains the variables to calculate the greek_fi

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import calculate_terms_test as terms

'''e = lngamma.kostas( 2, 2, 2, 2)
print ' the e = ', e'''


B = terms.B
print B


zmax , zmin = PR.Cubic_PR( ThT.T_System[i], ThT.P_System[0], am, bm )

y = zmax
x = zmin

print ' zmax root for the vapour phase = ', y
print ' zmin root for the liquid phase = ', x



def calc_chempot( iphase, NComp ):
    print ' am = ', terms.AM 
    
    return chempoti


# = = = = = = = = = # = = = = = = = = = # = = = = = = = = = # = = = = = = = = = #

Rconst = 8.314 # Gas constant [J/(gmol.K)]

ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp ) ]
print '  the initial molar fraction before reading from the input.dat is ', MFrac
# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase


    
