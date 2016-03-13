import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import pylab 
import time


Rconst = 8.314 # Gas constant [J/(gmol.K)]

ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp ) ];print '  the initial molar fraction before reading from the input.dat is', MFrac
# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase



# = = = = = = = = =  Gibbs energy in excess = Helmoltz energy in excess = = = = = = = = = =
def Gibbs( MFrac): 
    Gei = [ 0. for i in range( ThT.NComp ) ]
    for j in range(ThT.NComp):
        node = j * ThT.NComp + i
        for i in range(ThT.NComp):
            Gei = - ( Gei + MFrac[ j ] * ( np.log( MFrac [ i ] * ThT.Lamda_wilson[ node ] ) ) ) 
            print '  the Gibbs energy in excess for the component ', ThT.Species[i] ,' is Ge = ', Gei
        print
    return Ge




bm_nom = 0
bm_denom = 0
for i in range(ThT.NComp):
    bm_denom1 = 0
    for j in range(ThT.NComp):
        node = i * ThT.NComp + j
        bm_nom = bm_nom + ( MFrac[ i ] * MFrac[ j ] ) * ( PR.PREoS_Calc_b( i ) - PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] ) / ThT.Rconst * ThT.T_System[ 0 ] )
        bm_denom = 1 - 1 / ( ThT.Rconst * ThT.T_System[ 0 ] ) * ( MFrac[ i ] *  PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] ) / PR.PREoS_Calc_b( i )  )
    print '  the bm_nom = ', bm_nom
        
