#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EOS_PR as PR
import time


# = = = = = = = = = = = = = = = = = = = = = = = = = Reading Input data from external file = = = = = = = = = = = = = = = = = = = = = = = = = 
ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
print 'the molar fraction is', MFrac

#declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; MFrac[ 2 ] = 0.20 ; MFrac[ 3 ] = 0.2 # Vapour phase
MFrac[ 4 ] = 0.10; MFrac[ 5 ] = 0.10; MFrac[ 6 ] = 0.20 ; MFrac[ 7 ] = 0.6 # Liquid phase

print
print '  ---------------------------------------------------------------------------------------------------------------------'
# we have already declare the type and number of species from the input.dat
print '  The number of Species is: ', ThT.NComp , ' while the Species are : ', ThT.Species , ' and the number of phases are :', ThT.NPhase 
print '  ---------------------------------------------------------------------------------------------------------------------'


print
print '  Below we retrieve the characteristics/input data for each of the components and we loop over the .dat file we have'

a_sum = 0. ;b_sum = 0. # the initial sum for am and bm based on the eq. 2.7

aij = [0. for i in range(ThT.NComp**2) ] # the aij has the same dimension like the kij, a square matrix where the main diagonal is 0

for i in range(ThT.NComp):
    for j in range(ThT.NComp):
        print '  ===================================================================================================================== '
    	print '  for the  ', ThT.Species[i] 
    	print '  T_Crit = ', ThT.T_Crit[i]
    	print '  P_Crit = ', ThT.P_Crit[i]            
    	print '  wmega  = ', ThT.Accentric_Factor[i]    # accentric factor 
    	print '  Kij    = ', ThT.BinaryParameter[i]     # the binary parameter 
    	print '  xi     = ', ThT.MolarMass[i]           # the molar mass 
    	print '  zi     = ', ThT.Z_Feed[i]              # overall feed mass fraction of the component 
    	print ''
        
        # using the word node (pointer), that defines the componenet ij at the square matrix aij that has the same dimension like the kij
        node = i * ThT.NComp + j
        if i == j:                                                  # this is for the main diagonal
            aij[ node ] = PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] )  # I am calling the EOS_PR
            print '  you are at the vapour phase - 1 where  i = j ,', i, j
        else:                                                       # this is for the rest of the elements of the square matrix of the aij
            aij[ node ] = math.sqrt( PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] ) * PR.PREoS_Calc_a( j , ThT.T_System[ 0 ] ) ) * ( 1. - ThT.BinaryParameter[ node ] )
            print '  you are at the vapour phase - 1 where  i /= j ,', i, j
            a_sum = a_sum + aij[ node ] * MFrac[ i ] * MFrac[ j ]
            b_sum = b_sum + PR.PREoS_Calc_b(i) * MFrac[ i ]
            print '  at the ', i, j, ' the a_mixture = ', a_sum, ' and the b_mixture = ', b_sum
            print 
            time.sleep(2)                                           # this command gives the results every 2 sec

am = a_sum
bm = b_sum

print
print '  the a_mixture = ', a_sum
print '  the b_mixture = ', b_sum
print
     

# = = = = = = = = = = = = = = = = = = = = calculate EOS - PR   = = = = = = = = = = = = = = = = = = = = = = = = =
     
k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
alpha = ( 1. + k * ( 1. - math.sqrt( ThT.T_System[ 0 ] / ThT.T_Crit[i] ) )) **2
a_k = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
b_k = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]

print '  the a_k = ', a_k, ' the b_k = ', b_k

# = = = = = = = = = = = = = = = = = calculate the Cubic_PR( T, P, am, bm ) = = = = = = = = = = = = = = = = = = =
    
Big_A = am * ThT.P_System[ 0 ] / ( ThT.Rconst * ThT.T_System[ 0 ] )**2
Big_B = bm * ThT.P_System[ 0 ] / ( ThT.Rconst * ThT.T_System[ 0 ] )
coeffs = [0. for i in range( 4 ) ] # Coefficient of the polynomial
Z_root = [0. for i in range( 3 ) ] # Roots of the polynomial

coeffs[ 0 ] = 1.
coeffs[ 1 ] = - ( 1. - Big_B )
coeffs[ 2 ] = Big_A - 2. * Big_B - 3. * Big_B**2
coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 )
Z_root = np.roots( coeffs )       # Calculating the roots of the cubic eqn

np.set_printoptions(precision=6)  # Use set_printoptions to set the precision of the output
print '  the cubic root is = ', Z_root

   
# calculating the fugacity coef. for each component at each phase greek phi[i] = fi / Pxi, xi = molar fraction or ThT.MolarMass

# at this point i have to calculate the ai and bi 










def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max
    
    
              
 
    return GibbsEnergy

print
print 'kosta m@l@k@ as long as you see that the script goes through the lines!'



''' R, Tc, Pc, w all from the input.dat '''
