
#!/usr/bin/env python

import numpy as np
import math 
import sys
import ThermoTools as ThT

'''
      FUNCTIONS USED IN THE THERMODYNAMIC CALCULATIONS USING CUBIC EQUATIONS OF STATE
'''

# Given the component and temperature, this function will return
#    the attractive and repulsive parameters calculated based on this EoS
#    of this component:
def Cubic_EoS( icomp, Temp ):
    if ThT.EOS[ icomp ] == "Peng-Robinson" or ThT.EOS[ icomp ] == "PR" or \
            ThT.EOS[ icomp ] == "Peng-Robinson-Stryjek-Vera" or ThT.EOS[ icomp ] == "PRSV":
        ai = PR_EoS_Calc_a( icomp, Temp )
        bi = PR_EoS_Calc_b( icomp )
#
    else:
        sys.exit( 'For the time being, only PR and PRSV have been included into this code' )
#
    return ( ai, bi )



'''
       PENG-ROBINSON AND PENG-ROBINSON-STRYJECK-VERA
                    RELATED FUNCTIONS
'''

def PR_EoS_Calc_a( i, T ):
    if ThT.EOS[ i ] == "Peng-Robinson" or ThT.EOS[ i ] == "PR":
        k = 0.37464 + 1.5422 * ThT.Accentric_Factor[ i ] - 0.26992 * \
            ThT.Accentric_Factor[ i ]**2
    elif ThT.EOS[ i ] == "Peng-Robinson-Stryjek-Vera" or ThT.EOS[ i ] == "PRSV":
        k0 = 0.378893 + 1.4897153 * ThT.Accentric_Factor[ i ] - 0.17131848 * \
            ThT.Accentric_Factor[ i ]**2 + 0.0196554 * ThT.Accentric_Factor[ i ]**3
        k = k0 + ThT.EOS_K1[ i ] * ( 1. + math.sqrt( T / ThT.T_Crit[ i ] ) ) * \
            ( 0.7 - math.sqrt( T / ThT.T_Crit[ i ] ) )
    else:
        sys.exit( 'Appropriate EOS was not found' )

    alpha = ( 1. + k * ( 1. - math.sqrt( T / ThT.T_Crit[ i ] ) )) **2
    a_k = 0.45724 * ( ThT.RConst * ThT.T_Crit[ i ] )**2 / ThT.P_Crit[ i ] * alpha
    return a_k


def PR_EoS_Calc_b( i ): 
    b_k = 0.07780 * ThT.RConst * ThT.T_Crit[ i ]  / ThT.P_Crit[ i ]
    return b_k


###
### FUNCTION: Solving the Peng-Robinson EOS in the cubic form
###
def PR_Cubic( T, P, am, bm ):#, Zvapour, Zliquid ): # Building up and solving cubic EOS

    Big_A = am * P / ( ThT.RConst * T )**2
    Big_B = bm * P / ( ThT.RConst * T )

    if ThT.Debug:
        print 'Big A and B:', Big_A, Big_B

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

    Z_realroot = [ThT.Residual for i in range( 3 ) ] # Creating an array with very low values to receive the roots

    for i in range ( 3 ): 
        if abs( Z_root.imag[ i ] ) <= ThT.Residual: # Removing complex roots
            Z_realroot[ i ] = Z_root.real[ i ] # Extracting the real

    if ThT.Debug:
        print 'Real roots of Z:', Z_realroot



# In VLE problems the largest real root refers to the vapour phase, whereas the smallest real root is related
# to the liquid phase. For simplicity, let's consider only the largest root. For this, we can use a 'sort' 
# algorithm (see quicksort and/or bubblesort) within Numpy (np.sort).

    Z_realroot = np.sort( Z_realroot ) # Quick-sort algorithm -- Largest/smallest real root: vapour / liquid
    Zvapour = ThT.Residual ; Zliquid = 1. / ThT.Residual

    for i in range( 3 ):
        Zvapour = max( Zvapour, Z_realroot[ i ] ) # Largest real root: vapour
        Zliquid = min( Zliquid, Z_realroot[ i ] ) # Smallest real rrot: liquid

    return ( Zvapour, Zliquid ) 
    

'''
      FUNCTIONS USED IN THE THERMODYNAMIC CALCULATIONS USING XXXXXXXX
      EQUATION OF STATE
'''
