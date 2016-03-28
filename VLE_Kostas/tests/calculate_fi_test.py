#!/usr/bin/env python

# a function that contains the variables to calculate the greek_fi

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import calculate_terms_test as terms


def CALC_FI( iphase, frac ):
    lnfi = [ 0. for i in range( ThT.NComp ) ]
    zmax , zmin = PR.Cubic_PR( ThT.T_System[0], ThT.P_System[0], terms.AM( frac ), terms.BM( frac ) )

    DD_KC = [ 0. for i in range( ThT.NComp ) ]
    DD_KC = terms.DD( frac )
    DQ_KC = [ 0. for i in range( ThT.NComp ) ]
    DQ_KC = terms.DQ( frac )
    
    #for iphase in range( ThT.NPhase ):
    for i in range( ThT.NComp )  :
        print ' MFrac = ', frac[ i ]
        if iphase == 0:
           z = zmax
        else:
           z = zmin
     #   q = ( ( 1 / ( 1 - terms.D( frac ) ) * terms.DQ( frac ) - terms.Q( frac ) / ( 1 - terms.D( frac ) **2  ) * ( 1 - terms.DD( frac ) )
      #  q = ( ( 1 / ( 1 - terms.D( frac ) ) * terms.DQ( frac )[i] - terms.Q( frac ) / ( 1 - terms.D( frac ) **2  ) * ( 1 - terms.DD( frac )[i] )
        q = ( ( 1 / ( 1 - terms.D( frac ) ) * DQ_KC[i] - terms.Q( frac ) / ( 1 - terms.D( frac ) **2  ) * ( 1 - DD_KC[i] )
        d =  terms.D( frac ) * q + terms.BM( frac ) * ( 1 - terms.DD( frac ) )
        term1 = - np.log( z - terms.B( frac ) ) 
        term2 = ( 1 / terms.BM( frac ) ) * q * ( z - 1 )
        term3 = ( 0.5 / np.sqrt(2) ) * ( ( 1/ terms.AM( frac ) ) * d * ThT.Rconst * ThT.T_System[ 0 ] ) - ( ( 1 / terms.BM( frac ) ) * q ) 
        term4 = np.log( ( z / terms.B( frac )  + 1 - np.sqrt(2) ) / ( z / terms.B( frac )  + 1 + np.sqrt(2) ) )
        print ' term1 = ', term1, z, frac[ i ]
        print ' term2 = ', term2
        print ' term3 = ', term3
        print ' term4 = ', term4
        lnfi[ i ] = term1 + term2 + term3 * term4
        print '    fi = ', lnfi[ i ], ' for the MFrac = ', frac[ i ] 
    
    return lnfi


'''
def CALC_CHEMPOT( MFrac ):
    chempot = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
    for iphase in range( ThT.NPhase ): 
        for i in range( ThT.NComp )  :
            #print ' iphase = ', iphase, ' i = ', i   
            chempot[ i ] =  ThT.Rconst * ThT.T_System[ 0 ] * ( CALC_FI( MFrac[ i ] ) + np.log( ThT.P_System[ 0 ] * MFrac[ i ] ) )
        print ' iphase = ', iphase, ' i = ', i,' the chempot = ', chempot[ i ]
        print
    return chempot  
'''



ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp ) ]
print '  the initial molar fraction before reading from the input.dat is ', MFrac
# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase

iphase = 0

'''
zmax , zmin = PR.Cubic_PR( ThT.T_System[0], ThT.P_System[0], terms.AM( MFrac ), terms.BM( MFrac ) )
y = zmax
x = zmin
print ' zmax root for the vapour phase = ', y
print ' zmin root for the liquid phase = ', x
print
'''


phi = CALC_FI( iphase, MFrac )
print 'ln_phi = ', phi 

'''
chempot_mi = CALC_CHEMPOT( MFrac )
print ' greek_mi = ', chempot_mi 
'''



 




    
