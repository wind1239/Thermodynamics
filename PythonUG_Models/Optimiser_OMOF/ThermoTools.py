
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
    
