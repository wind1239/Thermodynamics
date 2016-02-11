#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EOS_PR as PR
import time
#import matplotlib.pyplot as plt


# = = = = = = = = = = = = = = = = = = = = = = = = = Reading Input data from external file = = = = = = = = = = = = = = = = = = = = = = = = = 
Rconst = 8.314 # Gas constant [J/(gmol.K)]

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

a_sum_V = 0. ;b_sum_V = 0. # the initial sum for am and bm based on the eq. 2.7 for Vapour phase
#a_sum_L = 0. ;b_sum_L = 0. # the initial sum for am and bm based on the eq. 2.7 for Liquid phase

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
            print '  you are at the vapour phase where  i = j ,', i, j
        else:                                                       # this is for the rest of the elements of the square matrix of the aij
            aij[ node ] = math.sqrt( PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] ) * PR.PREoS_Calc_a( j , ThT.T_System[ 0 ] ) ) * ( 1. - ThT.BinaryParameter[ node ] )
            print '  you are at the vapour phase where  i /= j ,', i, j
            a_sum_V = a_sum_V + aij[ node ] * MFrac[ i ] * MFrac[ j ]
            b_sum_V = b_sum_V + PR.PREoS_Calc_b(i) * MFrac[ i ]
            print '  at the Vapour Phase ', i, j, ' the a_mixture = ', a_sum_V, ' and the b_mixture = ', b_sum_V
            #a_sum_L = a_sum_L + aij[ node ] * MFrac[ i + 1 ] * MFrac[ j + 1 ]
            #b_sum_L = b_sum_L + PR.PREoS_Calc_b(i) * MFrac[ i +1 ] 
            #print '  at the Liquid Phase ', i+1 , j+1 , ' the a_mixture = ', a_sum_L, ' and the b_mixture = ', b_sum_L
            time.sleep(0)                                           # this command gives the results 

amv = a_sum_V
bmv = b_sum_V

#aml = a_sum_L
#bml = b_sum_L

print
print '  the a_mixture_Vapour = ', a_sum_V
print '  the b_mixture_Vapour = ', b_sum_V
print
#print '  the a_mixture_Liquid = ', a_sum_L
#print '  the b_mixture_Liquid = ', b_sum_L
#print    

# = = = = = = = = = = = = = = = = = = = = calculate EOS - PR   = = = = = = = = = = = = = = = = = = = = = = = = =
     
k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
alpha = ( 1. + k * ( 1. - math.sqrt( ThT.T_System[ 0 ] / ThT.T_Crit[i] ) )) **2
a_k = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
b_k = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]

print '  the a_k = ', a_k, ' the b_k = ', b_k
print 
# = = = = = = = = = = = = = = = = = calculate the Cubic_PR( T, P, am, bm ) = = = = = = = = = = = = = = = = = = =
    
Big_A = amv * ThT.P_System[ 0 ] / ( ThT.Rconst * ThT.T_System[ 0 ] )**2
Big_B = bmv * ThT.P_System[ 0 ] / ( ThT.Rconst * ThT.T_System[ 0 ] )
coeffs = [0. for i in range( 4 ) ] # Coefficient of the polynomial
Z_root = [0. for i in range( 3 ) ] # Roots of the polynomial

coeffs[ 0 ] = 1.
coeffs[ 1 ] = - ( 1. - Big_B )
coeffs[ 2 ] = Big_A - 2. * Big_B - 3. * Big_B**2
coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 )
Z_root = np.roots( coeffs )        # Calculating the roots of the cubic eqn

np.set_printoptions(precision=6)   # Use set_printoptions to set the precision of the output
print '  The EOS, written in terms of compressibility factor, has three real roots while the intermidiate root may be ignored.'
print '  The Min root = Liquid phase while the Max root = Vapour phase ' # PVT and Phase Behaviour Of Petroleum Reservoir Fluids Book By Ali Danesh
print '  the cubic root of the cubic_PR is = ', Z_root, 'while the Max = ', Z_root.max(), ' and the Min = ', Z_root.min() 
print
   
# = = = = = = = = = = = = = = = = = calculating the fugacity coef. = = = = = = = = = = = = = = = = =
# = = for each component at each phase greek phi[i] = fi / Pxi, xi = molar fraction or ThT.MolarMass
# = = = = = = = = = I am breaking down the fugacity coef. eq. into its terms = = = = = = = = = = = = 


# c term from the eq. 2.26
c = (1 / np.sqrt(2)) *  np.log( np.sqrt(2)-1 ) 
print '  the c = ', c
print


# calculate the Helmoltz free energy in excess Ae from 2.27 
Ae = 0
for i in range(ThT.NComp):
    Ae = c * ( ( amv / bmv ) - ( MFrac[ i ] * alpha / ( PR.PREoS_Calc_b(i) * Rconst * ThT.T_System[ 0 ] ) ) )
print '  the Helmoltz in excess Ae = ', Ae
print

# calculate the ln_gamma from 2.32
ln_gamma = 1 / ( Rconst * ThT.T_System[ 0 ] ) * Ae
print '  the ln_gamma = ', ln_gamma
print 

# D term from the eq. 2.17
D = 0
for i in range(ThT.NComp):
    D = D + ( MFrac[ i ] * alpha / ( PR.PREoS_Calc_b(i) * Rconst * ThT.T_System[ 0 ] ) ) + Ae * ( c * Rconst * ThT.T_System[ 0 ] )
print '  the D = ', D
print

# Q term from the eq. 2.28 (solving the 2.28 in respect of Q)
Q = (1 - D) * bmv
print '  the Q = ', Q
print

term1 = - np.log(Z_root.max() - Big_B)
print term1
print

term2 = (1 / b_sum_V) * ( alpha / ( PR.PREoS_Calc_b(i) * Rconst * ThT.T_System[ 0 ] ) + ( ln_gamma / c ) ) * (Z_root.max() - 1)
print term2
print

term3 = (1 / (2 * np.sqrt(2)) ) * ( a_sum_V / ( Rconst * ThT.T_System[ 0 ] * b_sum_V ))
print term3
print

term4 = ( 1 / amv ) * ( 2 - alpha / ( PR.PREoS_Calc_b(i) * Rconst * ThT.T_System[ 0 ] ) + ( ln_gamma / c ) )
print term4
print

term5 = np.log((Z_root.max() / Big_B + 1 - np.sqrt(2)) / (Z_root.max() / Big_B + 1 + np.sqrt(2)))
print term5
print

fugacity_coeff = term1 + term2+ term3 + term4 + term5

# calculate the ln(greek_fi) = fugacity_coeff
# greek_fi = e ^ fugacity_coeff
greek_fi = math.exp(fugacity_coeff)

print '  the fugacity_coeff a.k.a. greek_fi = ', greek_fi

for i in range(ThT.NComp):
    greek_mi  = ( Rconst * ThT.T_System[ 0 ] ) * ( fugacity_coeff + np.log( ThT.P_System[ 0 ] * MFrac[ i ] ) )
print '  the chemical potential a.k.a the greek_mi for the Vapour Phase !!! = ', greek_mi
print 


# = = = = = = = = = = = = = = = = =                              = = = = = = = = = = = = = = = = = =
# = = = = = = = = = = = = = = = = = Statement of the VLE Problem = = = = = = = = = = = = = = = = = =
# = = = = = = = = = = = = = = = = =                              = = = = = = = = = = = = = = = = = = 


print '  - -                                                                                                                     - - '
print '  - - Based on the things I have done so far the results above are for the Vapour Phase since we are using the Max Z_root - - '
print '  - -                                                                                                                     - - '



'''Rconst = 8.314 # Gas constant [J/(gmol.K)]
rho = ThT.P_System[ 0 ] / (Rconst * ThT.T_System[ 0 ] * Z_root)
#if plotcubic:
                          # Plot the cubic equation to visualize the roots
zz = np.linspace(0, 1.5)  # array for plotting
plt.figure()
plt.plot(zz, g(zz), color='k')
plt.xlabel('Compressibility, $z$')
plt.ylabel('Cubic $g(z)$')
plt.axvline(x=z)
plt.axhline(y=0)
plt.title('Root found @ z = %.2f' % z)
plt.show()
    #return {"density(mol/m3)": rho, "fugacity_coefficient": fugacity_coeff, "compressibility_factor": Z_root, "fugacity(bar)": fugacity_coeff * ThT.P_System[ 0 ], "molar_volume(L/mol)": 1.0 / rho * 1000.0} '''














def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max
    
    return GibbsEnergy

print
print 'kosta m@l@k@ as long as you see that the script goes through the lines!'



''' R, Tc, Pc, w all from the input.dat '''
