#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EOS_PR as PR

# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

print
print '---------------------------------------------------------------------------------------------------------------------------------'
# we have already declare the type and number of species from the input.dat
print 'The number of Species is: ', ThT.NComp , ' while the Species are : ', ThT.Species , ' and the number of phases are :', ThT.NPhase 
print '---------------------------------------------------------------------------------------------------------------------------------'
nc = ThT.NComp # the number of compounds/species that I will have to loop over

print
print 'Below we retrieve the T_crit , P_Crit and the accentric factor for each of the components '

for i in range(nc):
    print '====================================================================================================================================='
    print '  for the  ', ThT.Species[i] 
    print '  T_Crit = ', ThT.T_Crit[i]
    print '  P_Crit = ', ThT.P_Crit[i] 
    print '  wmega  = ', ThT.Accentric_Factor[i]  
    print '  Kij    = ', ThT.BinaryParameter[i]
    print '  xi     = ', ThT.MolarMass[i]
    print '  the initial feed composition is = ', ThT.Z_Feed[i]
    print ''

# calculating the fugacity coef. for each component at each phase greek phi[i] = fi / Pxi, xi = molar fraction or ThT.MolarMass

# at this point i have to calculate the ai and bi 
#                                       am and bm
#                                  then  A and B
#                                  then the cubic polynomial as below!


#==================================    EOS - PR   ==================================================
    def PREoS_Calc_a( i, T ): 
        k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
        alpha = ( 1. + k * ( 1. - math.sqrt( T / ThT.T_Crit[i] ) )) **2
        a_k = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
        return a_k

    def PREoS_Calc_b( i ): 
        ThT.set_Global_Variables()
        b_k = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]
        return b_k

    def Cubic_PR( T, P, am, bm ):
        ThT.set_Global_Variables()

        Big_A = am * P / ( ThT.Rconst * T )**2
        Big_B = bm * P / ( ThT.Rconst * T )
        coeffs = [0. for i in range( 4 ) ] # Coefficient of the polynomial
        Z_root = [0. for i in range( 3 ) ] # Roots of the polynomial

        coeffs[ 0 ] = 1.
        coeffs[ 1 ] = - ( 1. - Big_B )
        coeffs[ 2 ] = Big_A - 2. * Big_B - 3. * Big_B**2
        coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 )
        Z_root = np.roots( coeffs ) # Calculating the roots of the cubic eqn
        print Z_root




def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max
    
    
              
 
    return GibbsEnergy

''' R, Tc, Pc, w all from the input.dat '''
