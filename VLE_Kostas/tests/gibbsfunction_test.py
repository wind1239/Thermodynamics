#!/usr/bin/env python

#-----------------------------------------------------
# I can use methane at 65.0 bar and 298.0 K. 
# Methane has a Tc = -82.59 deg. C and Pc = 45.99 bar. 
# wmega = 0.011
#-----------------------------------------------------

#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import time
#import matplotlib.pyplot as plt

print
print
print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
print
print

# = = = = = = = = = = = = = = = = = = = = = = = = = Reading Input data from external file = = = = = = = = = = = = = = = = = = = = = = = = = 
Rconst = 8.314 # Gas constant [J/(gmol.K)]

ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
print '  the initial molar fraction before reading from the input.dat is', MFrac

#declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase

print
print '  ---------------------------------------------------------------------------------------------------------------------'
# we have already declare the type and number of species from the input.dat
print '  The number of Species is: ', ThT.NComp , ' while the Species are : ', ThT.Species , ' and the number of phases are :', ThT.NPhase 
print '  ---------------------------------------------------------------------------------------------------------------------'
print
print '  Below we retrieve the characteristics/input data for each of the components and we loop over the .dat file we have'

a_sum_V = 0. ;b_sum_V = 0.  # the initial sum for am and bm based on the eq. 2.7 for Vapour phase
#a_sum_L = 0. ;b_sum_L = 0. # the initial sum for am and bm based on the eq. 2.7 for Liquid phase

aij = [0. for i in range(ThT.NComp**2) ] # the aij has the same dimension like the kij, a square matrix where the main diagonal is 0
Lij = [1. for i in range(ThT.NComp**2) ]
ln_gamma = [0. for i in range(ThT.NComp**2) ]


for iphase in range(1):
    print
    print '                                                                                               you are at the iphase ', iphase 
    print
    for i in range(ThT.NComp):
        for j in range(ThT.NComp):
            node = i * ThT.NComp + j
            print '  ===================================================================================================================== '
    	    print '  you are at the component ' , i, j, ' at the node = ', node, 'at the ', iphase,' phase'
            print 
            print '  for the  ', ThT.Species[i] 
    	    print '  T_Crit = ', ThT.T_Crit[i]
    	    print '  P_Crit = ', ThT.P_Crit[i]            
    	    print '  wmega  = ', ThT.Accentric_Factor[i]       # accentric factor 
    	    print '  Kij    = ', ThT.BinaryParameter[node]     # the binary parameter 
    	    print '  Lij    = ', ThT.Lamda_wilson[node]        # the lamda parameter for the ln_gamma term
            print '  xi     = ', ThT.MolarMass[i]              # the molar mass 
    	    print '  zi     = ', ThT.Z_Feed[i]                 # overall feed mass fraction of the component 
    	    print ''
        
            if i == j:                                                  # this is for the main diagonal
               aij[ node ] = PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] )   # I am calling the EOS_PR
               print '  aij = ', aij[ node ]
               print '  MFrac = ', MFrac[ i ]
               ln_gamma[ node ] = lng.ln_gamma( MFrac )
               print '  the ln_gamma = ', ln_gamma[ node ] 
               print
               print 
            else:                                              # this is for the rest of the elements of the square matrix of the aij
               aij[ node ] = math.sqrt( PR.PREoS_Calc_a( i , ThT.T_System[ 0 ] ) * PR.PREoS_Calc_a( j , ThT.T_System[ 0 ] ) ) * ( 1. - ThT.BinaryParameter[ node ] )
               print '  aij = ', aij[ node ]   
               print '  MFrac = ', MFrac[ i ]
               ln_gamma[ node ] = lng.ln_gamma( MFrac )
               print
               print 
               a_sum_V = a_sum_V + aij[ node ] * MFrac[ i ] * MFrac[ j ]
               b_sum_V = b_sum_V + PR.PREoS_Calc_b(i) * MFrac[ i ]
               print '  at the Vapour Phase ', i, j, ' the a_mixture = ', a_sum_V, ' the b_mixture = ', b_sum_V, 
               print
               #a_sum_L = a_sum_L + aij[ node ] * MFrac[ i + 1 ] * MFrac[ j + 1 ]
               #b_sum_L = b_sum_L + PR.PREoS_Calc_b(i) * MFrac[ i +1 ] 
               #print '  at the Liquid Phase ', i+1 , j+1 , ' the a_mixture = ', a_sum_L, ' and the b_mixture = ', b_sum_L
               time.sleep(0)                                           # this command gives the results 
         
            
     

amv = a_sum_V
bmv = b_sum_V

#aml = a_sum_L
#bml = b_sum_L

print '  - - - - - - - - - - - - - - - - - - - '  
print '  the a_mixture_Vapour = ', amv
print '  the b_mixture_Vapour = ', bmv
print '  - - - - - - - - - - - - - - - - - - - '
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
print '  the cubic root of the cubic_PR is = ', Z_root,  
print '  the Max = ', Z_root.max(), ' and the Min = ', Z_root.min()
print

print '  = = = = = =                                                                                              = = = = = = '
print '  = = = =                                   calculating the fugacity coef.                                     = = = = '
print '  = =         for each component at each phase greek phi[i] = fi / Pxi, xi = molar fraction or ThT.MolarMass       = = '
print '  = = = =                    I am breaking down the fugacity coef. eq. into its terms                          = = = = ' 
print '  = = = = = =                                                                                              = = = = = = '
print
print


c = (1 / np.sqrt(2)) * np.log( np.sqrt(2)-1 )                      # c term from the eq. 2.26
D = amv/ ( Rconst * ThT.T_System[ 0 ] * bmv )                      # D and Q term from  the eq. 2.28
Q = bmv * ( 1- D )
print '  the terms D = ', D, ' Q = ', Q, ' c = ', c  
print


# = = = = = = = = = = = calculate each term to find the ln_greek_fi a.k.a. fugacity coef. = = = = = = = = = = = 

# = = = = = = = = = = = = = = = = = = term 1 = = = = = = = = = = = = = = = = = = = = = = = =
term1 = - np.log(Z_root.max() - Big_B)
print '  the term 1 = ',term1
print


# = = = = = = = = = = = = = = = = = = term 2 = partial1 = = = = = = = = = = = = = = = = = =
# = = = = calculate the ln_gamma based on the lamdaij you have from the input data  = = = = 
sum_partial1_1 = 0
sum_partial1_2 = 0
for i in range(ThT.NComp):
    for j in range(ThT.NComp):
        sum_partial1_1 = sum_partial1_1 + 2 * np.sum( MFrac[i] * ( PR.PREoS_Calc_b(i)  - ( alpha / Rconst * ThT.T_System[ 0 ] ) ) )   
    sum_partial1_2 = ( alpha / ( PR.PREoS_Calc_b(i) * Rconst * ThT.T_System[ 0 ] ) + lng.ln_gamma( MFrac ) / c ) 
    #print sum_partial1_1
    #print sum_partial1_2
partial1 = sum_partial1_1 / (1 - D) + ( Q * ( 1- sum_partial1_2 ) ) / ( ( 1 - D )**2 )

term2 = ( 1 / bmv ) * partial1 * (Z_root.max() - 1)

print '  the term 2 = ', term2
print

# = = = = = = = = = = = = = = = = = = term 3 = = = = = = = = = = = = = = = = = = = = = = = =
term3 = (1 / (2 * np.sqrt(2)) ) * ( a_sum_V / ( Rconst * ThT.T_System[ 0 ] * b_sum_V ) ) * (  ( 1 / a_sum_V ) * ( D * partial1  +  bmv * sum_partial1_2 ) * ( Rconst * ThT.T_System[ 0 ] ) - ( ( 1 / b_sum_V ) * partial1 )   )
print '  the term 3 = ',term3
print


# = = = = = = = = = = = = = = = = = = term 4 = = = = = = = = = = = = = = = = = = = = = = = =
term4 = np.log((Z_root.max() / Big_B + 1 - np.sqrt(2)) / (Z_root.max() / Big_B + 1 + np.sqrt(2)))
print '  the term 4 = ',term4
print

fugacity_coeff = term1 + term2+ term3 * term4 

# calculate the ln(greek_fi) = fugacity_coeff
# greek_fi = e ^ fugacity_coeff
#greek_fi = math.exp(fugacity_coeff)

print '  the fugacity_coeff a.k.a. ln_greek_fi = ', fugacity_coeff

for i in range(ThT.NComp):
    greek_mi  = ( Rconst * ThT.T_System[ 0 ] ) * ( fugacity_coeff + np.log( ThT.P_System[ 0 ] * MFrac[ i ] ) )
print '  the chemical potential a.k.a the greek_mi for the Vapour Phase !!! = ', greek_mi
print 

Gei = 0
for i in range(ThT.NComp):
    Gei = Gei + MFrac[ i ] * np.log( MFrac [ i ] * ThT.Lamda_wilson[ i ] ) 
    print '  the Gibbs energy in excess for the component ', ThT.Species[i] ,' is Gei = ', Gei 
print

# = = = = = = = = = = = = = = = = =                              = = = = = = = = = = = = = = = = = =
# = = = = = = = = = = = = = = = = = Statement of the VLE Problem = = = = = = = = = = = = = = = = = =
# = = = = = = = = = = = = = = = = =                              = = = = = = = = = = = = = = = = = = 


#print '  - -                                                                                                                     - - '
#print '  - - Based on the things I have done so far the results above are for the Vapour Phase since we are using the Max Z_root - - '
#print '  - -                                                                                                                     - - '





'''
def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max
    
    return GibbsEnergy
print'''

# -------------------------------------------------------------------------------
print
print '  --------------------------------------------------------------------------- '
print '  kosta m@l@k@ as long as you see that the script goes through all the lines! '
print '  --------------------------------------------------------------------------- '
print
# -------------------------------------------------------------------------------

''' R, Tc, Pc, w all from the input.dat ''' 
