#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
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

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ];print '  the initial molar fraction before reading from the input.dat is', MFrac

# declare a vector with MFrac values - molar fraction
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
Ln_Gamma = [0. for i in range(ThT.NComp**2) ]
bart = [0. for i in range(ThT.NComp**2) ]
bm = 0
am = 0

for i in range(ThT.NComp):
    for j in range(ThT.NComp):
        node = i * ThT.NComp + j
        print '  ===================================================================================================================== '
    	print '  you are at the component ' , i, j, ' at the node = ', node 
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
            print '  MFrac = ', MFrac[ node ]
            k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
            alpha = ( 1. + k * ( 1. - math.sqrt( ThT.T_System[ 0 ] / ThT.T_Crit[i] ) )) **2
            a_i = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
            b_i = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]
            ln_gamma[ node ] = 1 - np.log ( np.inner( MFrac , ThT.Lamda_wilson ) ) - np.sum( ( MFrac[ i ] * ThT.Lamda_wilson[ j ] ) / ( np.inner( MFrac , ThT.Lamda_wilson ) )   )
            Ge =  - ( ThT.Rconst * ThT.T_Crit[i] ) * ( np.inner( MFrac, np.log( ( np.inner( MFrac , ThT.Lamda_wilson ) ) ) ) )
            print '  the ln_gamma = ', ln_gamma[ node ],'  the k = ', k,'  the alphai = ', alpha, '  the ai = ', a_i, '  the b_i = ', b_i  
            print '  for the component ', ThT.Species[i],' with respect to its self the Ge = ', Ge
            print 
        else:                                                       # this is for the rest of the elements of the square matrix of the aij
            print '  MFrac = ', MFrac[ i ]
            ln_gamma[ node ] = 1 - np.log ( np.inner( MFrac , ThT.Lamda_wilson ) ) - np.sum( ( MFrac[ i ] * ThT.Lamda_wilson[ j ] ) / ( np.inner( MFrac , ThT.Lamda_wilson ) )   )
            Ge =  - ( ThT.Rconst * ThT.T_Crit[i] ) * ( np.inner( MFrac, np.log( ( np.inner( MFrac , ThT.Lamda_wilson ) ) ) ) )
            k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
            alpha = ( 1. + k * ( 1. - math.sqrt( ThT.T_System[ 0 ] / ThT.T_Crit[i] ) )) **2
            a_i = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
            b_i = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]
            bart[ node ] = ( b_i - ( a_i / ThT.Rconst * ThT.T_Crit[i]) ) + ( b_i - ( a_i / ThT.Rconst * ThT.T_Crit[i]) ) * ( 1 - ThT.BinaryParameter[node] ) / 2
            bm_nominator = MFrac[ i ] * MFrac[ j ] * ( bart[ node ] ) 
            bm_denominator =  1 - 1 / ( ThT.Rconst * ThT.T_Crit[i] ) - Ge + np.inner( MFrac , ( a_i/b_i ) )
            bm = bm + bm_nominator / bm_denominator
            am = bm * ( bm_denominator )
            print '  the ln_gamma = ', ln_gamma[ node ],'  the k = ', k,'  the alphai = ', alpha, '  the ai = ', a_i, '  the b_i = ', b_i  
            print '  the bart = ', bart[ node ], ' the bm = ', bm, ' the am = ', am
            print '  for the component ', ThT.Species[i],' with respect to' , ThT.Species[j],'  the Ge = ', Ge
            print
            print 
            time.sleep(0)                                           # this command gives the results 
         
            
print '  - - - - - - - - - - - - - - - - - - - '  
bm = np.sum(bm); print '  the final am = ', bm
am = np.sum(am); print '  the final bm = ', am
print '  - - - - - - - - - - - - - - - - - - - '
print 

# = = = = = = = = = = = = = = = = = calculate the Cubic_PR( T, P, am, bm ) = = = = = = = = = = = = = = = = = = =
    
Big_A = am * ThT.P_System[ 0 ] / ( ThT.Rconst * ThT.T_System[ 0 ] )**2
Big_B = bm * ThT.P_System[ 0 ] / ( ThT.Rconst * ThT.T_System[ 0 ] )
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


c = (1 / np.sqrt(2)) * np.log( np.sqrt(2)-1 )                    # c term from the eq. 2.26
D = bm_denominator                                               # D and Q term from  the eq. 2.28
Q = bm_nominator
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
    sum_partial1_2 = ( alpha / ( PR.PREoS_Calc_b(i) * Rconst * ThT.T_System[ 0 ] ) + Ln_Gamma / c ) 
    
#print sum_partial1_1
#print sum_partial1_2

partial1 = sum_partial1_1 / (1 - D) + ( Q * ( 1- sum_partial1_2 ) ) / ( ( 1 - D )**2 )

term2 = ( 1 / bm ) * partial1 * (Z_root.max() - 1)

print '  the term 2 = ', term2
print

# = = = = = = = = = = = = = = = = = = term 3 = = = = = = = = = = = = = = = = = = = = = = = =
#term3 = (1 / (2 * np.sqrt(2)) ) * ( a_sum_V / ( Rconst * ThT.T_System[ 0 ] * b_sum_V ) ) * (  ( 1 / a_sum_V ) * ( D * partial1  +  bmv * sum_partial1_2 ) * ( Rconst * ThT.T_System[ 0 ] ) - ( ( 1 / b_sum_V ) * partial1 )   )
term3 = 0
print '  the term 3 = ',term3
print


# = = = = = = = = = = = = = = = = = = term 4 = = = = = = = = = = = = = = = = = = = = = = = =
term4 = np.log((Z_root.max() / Big_B + 1 - np.sqrt(2)) / (Z_root.max() / Big_B + 1 + np.sqrt(2)))
print '  the term 4 = ',term4
print

fugacity_coeff = term1 + term2+ term3 * term4 
print '  the fugacity_coeff a.k.a. ln_greek_fi = ', fugacity_coeff

for i in range(ThT.NComp):
    greek_mi  = ( Rconst * ThT.T_System[ 0 ] ) * ( fugacity_coeff + np.log( ThT.P_System[ 0 ] * MFrac[ i ] ) )
print '  the chemical potential a.k.a the greek_mi for the Vapour Phase ! ', greek_mi
print 


print '  - -                                                                                                                     - - '
print '  - - Based on the things I have done so far the results above are for the Vapour Phase since we are using the Max Z_root - - '
print '  - -                                                                                                                     - - '









def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max
    
    return GibbsEnergy

print
print 'kosta m@l@k@ as long as you see that the script goes through the lines!'



