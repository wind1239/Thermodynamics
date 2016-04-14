
#!/usr/bin/env python

import numpy as np
import math 
import thermotools_test as ThT


###
### FUNCTION: Calculating parameter of binary attraction of Peng-Robinson
###
def PREoS_Calc_a( i, T ): 
    k = 0.37464 + 1.5422 * ThT.Accentric_Factor[i] - 0.26992 * ThT.Accentric_Factor[i]**2
    alpha = ( 1. + k * ( 1. - math.sqrt( T / ThT.T_Crit[i] ) )) **2
    a_k = 0.45724 * ( ThT.Rconst * ThT.T_Crit[i] )**2 / ThT.P_Crit[i] * alpha
    return a_k


###
### FUNCTION: Calculating parameter of binary repulsion of Peng-Robinson
###
def PREoS_Calc_b( i ): 
#    ThT.set_Global_Variables()

    b_k = 0.07780 * ThT.Rconst * ThT.T_Crit[i]  / ThT.P_Crit[i]

    return b_k


###
### FUNCTION: Solving the Peng-Robinson EOS in the cubic form
###
def Cubic_PR( T, P, am, bm ):#, Zvapour, Zliquid ): # Building up and solving cubic EOS
    #ThT.set_Global_Variables()

    Big_A = am * P / ( ThT.Rconst * T )**2
    Big_B = bm * P / ( ThT.Rconst * T )

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
    coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 ); print coeffs, am, bm
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
    Zvapour = 1.e-6 ; Zliquid = 1.e6

    for i in range( 3 ):
        Zvapour = max( Zvapour, Z_realroot[ i ] ) # Largest real root: vapour
        Zliquid = min( Zliquid, Z_realroot[ i ] ) # Smallest real rrot: liquid

    return ( Zvapour, Zliquid ) 
    
