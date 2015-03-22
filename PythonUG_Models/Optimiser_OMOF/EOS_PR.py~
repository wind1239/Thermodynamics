
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions


# =================== ASSOCIATED/EXTERNAL FUNCTIONS ====================#


###
### FUNCTION: Global Variables
###

def set_Global_Variables(): # Global variables
# Building up a small data bank for species C1 and C2:
    global Rconst, Nc, species, MW, Tc, Pc, w, FeedMoleFraction,kij

    Rconst = 8.314 # Gas constant [J/(gmol.K)]

    Nc = 2 # Number of chemical species

    species = np.array( [ 'C1', 'C2' ] ) # List of chemical species

    MW = np.array( [ 16.043, 30.07 ] ) # Molar mass (in g/gmol)

    Tc = np.array( [ 190.4, 305.4 ] ) # Critical temperature (in K)

    Pc = np.array( [ 46., 48.8 ] ) # Critical pressure (in bar)
    Pc = Pc * 1.e5 # (converting into Pa)

    w = np.array( [ 0.011, 0.099 ] ) # Accentric factor

    FeedMoleFraction = np.array( [ 0.45, 0.55 ] ) # Feeding mole fraction

# Binary Interaction Parameter: this is stored as a tensor with indices 'i'and 'j': k(i,j). Thus for 3 components:
#    k11  k12  k13
#    k21  k22  k23
#    k31  k32  k33
# with k(i,i) = k(j,j) = 0. and k(i,j) = k(j,i). The tensor (or matrix in this case) is stored as a simple array k( node )
# with coordinates: node = i * Nc + j 
# Therefore we just need to detrmine k(i,j). For 2 components
#    k11  k12
#    k21  k22
# with k11 = k22 = 0. and
    kij = [0. for i in range( Nc**2 ) ]
    i = 0 ; j = 1 ; node  = i * Nc + j 
    i = 1 ; j = 0 ; node2 = i * Nc + j 
    kij[ node ] = 0.02 # THIS VALUE NEEDS TO BE UPDATED ...
    kij[ node2 ] = kij[ node ]
    

###
### FUNCTION: Calculating parameter of binary attraction of Peng-Robinson
###
def PREoS_Calc_a( i, T ): 
    set_Global_Variables()
    k = 0.37464 + 1.5422 * w[i] - 0.26992 * w[i]**2
    alpha = ( 1. + k * ( 1. - math.sqrt( T / Tc[i] ) )) **2
    a_k = 0.45724 * ( Rconst * Tc[i] )**2 / Pc[i] * alpha
    return a_k


###
### FUNCTION: Calculating parameter of binary repulsion of Peng-Robinson
###
def PREoS_Calc_b( i ): 
    set_Global_Variables()

    b_k = 0.07780 * Rconst * Tc[i]  / Pc[i]

    return b_k


###
### FUNCTION: Calculation of attraction and repulsion parameters for a simple mixing rule (quadratic and linear formulation for
###           attraction and repulsion, respectively)
###
def MixingRules(T):
    set_Global_Variables()

    aij = [0. for i in range( Nc**2 ) ] # Parameter for the combining rule (stored in the same way as kij)

    sum1 = 0. ; sum2 = 0.
    for i in range( Nc ):
        for j in range( Nc ):
            node = i * Nc + j 
            if i == j:
                aij[ node ] = PREoS_Calc_a( i , T )
            else:
                aij[ node ] = math.sqrt( PREoS_Calc_a( i , T ) * PREoS_Calc_a( j , T ) ) * ( 1. - kij[ node ] )

            sum1 = sum1 + aij[ node ] * FeedMoleFraction[ i ] * FeedMoleFraction[ j ]
        sum2 = sum2 + PREoS_Calc_b( i ) * FeedMoleFraction[ i ]
    am = sum1 ; bm = sum2 # am and bm (mixing rules)

    return am, bm

###
### FUNCTION: Solving the Peng-Robinson EOS in the cubic form
###
def Cubic_PR( T, P, am, bm ): # Building up and solving cubic EOS
    set_Global_Variables()

    Big_A = am * P / ( Rconst * T )**2
    Big_B = bm * P / ( Rconst * T )

# For a polynomial of order n:
#  c[0]*Z**n + c[1]*Z**(n-1) + ... + c[n] = 0
# We can use the Numpy internal function -- np.roots -- to calculate the root of the polynomial.  For a cubic polynomial:
# c[0] * Z**3 + c[1] * Z**2 + c[2] * Z**1 + c[3] * Z**0 = 0 
# This results in three roots for the polynomial, i.e., Z1, Z2 and Z3
    coeffs = [0. for i in range( 4 ) ] # Coefficient of the polynomial
    Z_root = [0. for i in range( 3 ) ] # Roots of the polynomial

    coeffs[ 0 ] = 1.
    coeffs[ 1 ] = - ( 1. - Big_B )
    coeffs[ 2 ] = Big_A - 2. * Big_B - 3. * Big_B**2
    coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 )
    Z_root = np.roots( coeffs ) # Calculating the roots of the cubic eqn

# Roots of a cubic equation can be either real or complex.  np.roots returns roots of format: a + bj, 
# where 'a' is the real part and 'b' is the imaginary part. However, we are only interested on the real roots. 

    smallvalue = -1.e15
    Z_realroot = [smallvalue for i in range( 3 ) ] # Creating an array with very low values to receive the roots

    for i in range ( 3 ): 
        if abs( Z_root.imag[ i ] ) <= 1.e-7: # Removing complex roots
            Z_realroot[ i ] = Z_root.real[ i ] # Extracting the real


# In VLE problems the largest real root refers to the vapour phase, whereas the smallest real root is related
# to the liquid phase. For simplicity, let's consider only the largest root. For this, we can use a 'sort' 
# algorithm (see quicksort and/or bubblesort) within Numpy (np.sort).

    Z_realroot = np.sort( Z_realroot ) # Quick-sort algorithm -- Largest/smallest real root: vapour / liquid
    Zvapour = Z_realroot[ 2 ] # Largest real root: vapour
    return Zvapour 
    



#=============================== MAIN CODE ==================================#

print 'Solving Peng-Robinson for a mixture of fluids using simple mixing rules'
set_Global_Variables()

print ' '
print 'Fluid \t\t  MW (g/gmol) \t  Tc (K) \t  Pc (Pa) \t  w \t\t    x  '
for index in range( Nc ):
    print species[ index ],'\t\t', MW[ index ], '\t\t',  Tc[ index ], '\t\t', Pc[ index ], '\t', w[ index ], '\t\t', FeedMoleFraction[ index ]
print ' '

print 'Binary interaction parameter (Kij):', kij[1]
print ' '

MixingParameters = [0 for i in range(2) ]
T = 300. ; P = 1.e7
print 'T:', T,'K and P:', P/1.e5, 'bar'

MixingParameters = MixingRules(T) # Calculating am and bm through simple mixing rules based on individual attractive and repulsive binary parameters

Zvapour = Cubic_PR( T, P, MixingParameters[0], MixingParameters[1] )
print ' '

print 'Zvapour: ', Zvapour

# Now, add here functions to calculate molar volume of the gaseous mixture and specific volume of chemical species at reservoir conditions
