
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions


# =================== ASSOCIATED/EXTERNAL FUNCTIONS ====================#


###
### Global Variables
###

def set_Global_Variables(): # Global variables
    global Rconst, Nc, species, MW, Tc, Pc, w, FeedMoleFraction
    Rconst = 8.314 # J/(gmol.K)

# Building up a small data bank for species C1 and C2:
    Nc = 2 # Number of chemical species
    species = np.array( [ 'C1', 'C2' ] ) # Chemical species
    MW = np.array( [ 16.043, 30.07 ] )
    Tc = np.array( [ 190.4, 305.4 ] ) # Creating array with critical tenmperature (in K)
    Pc = np.array( [ 46., 48.8 ] ) # Creating array with critical pressure (in bar)
    Pc = Pc * 1.e5 # (in Pa)
    w = np.array( [ 0.011, 0.099 ] ) # Creating accentric factor
    FeedMoleFraction = np.array( [ 0.45, 0.55 ] )

    
    
def PREoS_Calc_a( i, T ): # Calculating parameter of binary attraction
    set_Global_Variables()
    k = 0.37464 + 1.5422 * w[i] - 0.26992 * w[i]**2
    alpha = ( 1. + k * ( 1. - math.sqrt( T / Tc[i] ) )) **2
    a_k = 0.45724 * ( Rconst * Tc[i] )**2 / Pc[i] * alpha
    return a_k


def PREoS_Calc_b( i ): # Calculating parameter of binary repulsion
    set_Global_Variables()
    b_k = 0.07780 * Rconst * Tc[i]  / Pc[i]
    return b_k


def MixingRules(T):
    set_Global_Variables()
    aij = [0. for i in range( Nc**2 ) ] # Parameter for the combining rule
    kij = [0. for i in range( Nc**2 ) ] # Binary Interaction Parameter 

    sum1 = 0. ; sum2 = 0.
    for i in range( Nc ):
        for j in range( Nc ):
            node = ( i - 1 ) * Nc + j - 1
            if i == j:
                aij[ node ] = PREoS_Calc_a( i , T )
            else:
                kij[ node ] = 0.02 # THIS VALUE NEEDS TO BE UPDATED ...
                aij[ node ] = math.sqrt( PREoS_Calc_a( i , T ) * PREoS_Calc_a( j , T ) ) * ( 1. - kij[ node ] )

            sum1 = sum1 + aij[ node ] * FeedMoleFraction[ i ] * FeedMoleFraction[ j ]
        sum2 = sum2 + PREoS_Calc_b( i ) * FeedMoleFraction[ i ]
    am = sum1 ; bm = sum2 # am and bm (mixing rules)
    return am, bm

def Cubic_PR( T, P, am, bm ): # Building up and solving cubic EOS
    set_Global_Variables()
    Big_A = am * P / ( Rconst * T )**2
    Big_B = bm * P / ( Rconst * T )
    coeffs = [0. for i in range( 4 ) ]  # creating an array of coefficients for the polyn c[0]*Z**n + c[1]*Z**(n-1) + ... + c[n]
    Z_root = [0. for i in range( 3 ) ]
    smallvalue = -1.e12
    Z_realroot = [smallvalue for i in range( 3 ) ]
    coeffs[ 0 ] = 1.
    coeffs[ 1 ] = - ( 1. - Big_B )
    coeffs[ 2 ] = Big_A - 2. * Big_B - 3. * Big_B**2
    coeffs[ 3 ] = - ( Big_A * Big_B - Big_B**3 - Big_B**2 )
    Z_root = np.roots( coeffs ) # Calculating the root of the cubic eqn
    for i in range ( 3 ): # Removing complex roots ...
        if abs( Z_root.imag[ i ] ) <= 1.e-7:
            Z_realroot[ i ] = Z_root.real[ i ]
    Z_realroot = np.sort( Z_realroot ) # Quick-sort algorithm -- Largest/smallest real root: vapour / liquid
    Zvapour = Z_realroot[ 2 ] # Largest real root: vapour
    print 'Zvapour: ', Zvapour
    return Zvapour 
    



#=============================== MAIN CODE ==================================#

print 'Solving Peng-Robinson for a mixture of fluids using simple mixing rules at'
set_Global_Variables()

MixingParameters = [0 for i in range(2) ]
T = 300. ; P = 1.e7
print 'T:', T,'K and P:', P/1.e5, 'bar'

MixingParameters = MixingRules(T) # Calculating am and bm through simple mixing rules based on individual attractive and repulsive binary parameters
Zvapour = 0.
#Cubic_PR( T, P, MixingParameters[0], MixingParameters[1] )

Zvapour = Cubic_PR( T, P, MixingParameters[0], MixingParameters[1] )
print 'Zvapour: ', Zvapour
