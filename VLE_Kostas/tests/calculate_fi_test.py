#!/usr/bin/env python

# a function that contains the variables to calculate the greek_fi

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import calculate_terms_test as terms


def CALC_FI( MFrac ):
    fi = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
    zmax , zmin = PR.Cubic_PR( ThT.T_System[0], ThT.P_System[0], terms.AM( MFrac ), terms.BM( MFrac ) )
    for iphase in range( ThT.NPhase ):
        for i in range( ThT.NComp )  :
            node = iphase * ThT.NPhase + i 
            print ' MFrac =', MFrac[ node ]
            if iphase == 0:
                z = zmax
            else:
                z = zmin
            q = 1 #( 1 / ( 1 - terms.D( node ) ) * terms.DQ( MFrac ) - terms.Q( MFrac ) / ( 1 - terms.D( MFrac ) **2  ) * ( 1 - terms.DD( MFrac ) )
            d =  terms.D( MFrac ) * q + terms.BM( MFrac ) * ( 1 - terms.DD( MFrac ) )
            term1 = - np.log( z - terms.B( MFrac ) ) 
            term2 = ( 1 / terms.BM(MFrac) ) * q * ( z - 1 )
            term3 = ( 0.5 / np.sqrt(2) ) * ( ( 1/ terms.AM( MFrac ) ) * d * ThT.Rconst * ThT.T_System[ 0 ] ) - ( ( 1 / terms.BM( MFrac ) ) * q ) 
            term4 = np.log( ( z / terms.B( MFrac )  + 1 - np.sqrt(2) ) / ( z / terms.B( MFrac )  + 1 + np.sqrt(2) ) )
            print ' term1 = ', term1, z, MFrac[ node ]
            print ' term2 = ', term2
            print ' term3 = ', term3
            print ' term4 = ', term4
        fi[ node ] = term1 + term2 + term3 * term4
    print '    fi = ', fi[ node ], ' for the MFrac = ', MFrac[ node ]
    return fi


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

'''
zmax , zmin = PR.Cubic_PR( ThT.T_System[0], ThT.P_System[0], terms.AM( MFrac ), terms.BM( MFrac ) )
y = zmax
x = zmin
print ' zmax root for the vapour phase = ', y
print ' zmin root for the liquid phase = ', x
print
'''


phi = CALC_FI( MFrac )
print 'ln_phi = ', phi 

'''
chempot_mi = CALC_CHEMPOT( MFrac )
print ' greek_mi = ', chempot_mi 
'''



 




    
