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

MFrac = [ 0. for i in range( ThT.NComp ) ]
print '  the initial molar fraction before reading from the input.dat is', MFrac
# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase



bart = [0. for i in range(ThT.NComp**2) ]
bm = 0
am = 0

bm_nom = 0
bm_denom = 0
for i in range(ThT.NComp):
    for j in range(ThT.NComp):
        node = i * ThT.NComp + j
        k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
        alpha = ( 1. + k * ( 1. - math.sqrt( ThT.T_System[ 0 ] / ThT.T_Crit[i] ) )) **2
        a_i = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
        b_i = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]
        bart[ node ] = ( b_i - ( a_i / ThT.Rconst * ThT.T_Crit[i]) ) + ( b_i - ( a_i / ThT.Rconst * ThT.T_Crit[i]) ) * ( 1 - ThT.BinaryParameter[node] ) / 2
        bm_nominator = MFrac[ i ] * MFrac[ j ] * ( bart[ node ] ) 
        bm_denominator =  1 - 1 / ( ThT.Rconst * ThT.T_Crit[i] ) - lng.gibbs( MFrac ) + np.inner( MFrac , ( a_i/b_i ) )
        bm = bm + bm_nominator / bm_denominator
        am = bm * ( bm_denominator )
        print ' ----------- the component ', ThT.Species[i],' with respect to' , ThT.Species[j], '---------------- '    
        print '  the k = ', k,'  the alphai = ', alpha, '  the ai = ', a_i, '  the bi = ', b_i  
        print '  the bart = ', bart[ node ], ' the bm = ', bm, ' the am = ', am
        print 
    print 
         

def Q(MFrac):
    Q = bm_nominator
    print ' Q = ', Q
    return Q

def D(Mfrac):
    D = bm_denominator
    print ' D = ', D
    return D

c = (1 / np.sqrt(2)) * np.log( np.sqrt(2)-1 )                    # c term from the eq. 2.26
print '  the c = ', c
print
        
