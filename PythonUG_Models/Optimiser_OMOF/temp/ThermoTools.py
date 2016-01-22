
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math
import re
import sys
import fileinput


# =================== ASSOCIATED/EXTERNAL FUNCTIONS ====================#


###
### FUNCTION: Global Variables
###

def ReadSet_Global_Variables(): # Read variables from a external file called 'input.dat'
    import csv # Using csv (comma separated values) module
 
    global Rconst, Ncomp, T_Crit, P_Crit, MolarMass, Components, Accentric_Factor, \
        Z_Feed, BinaryParameter

    Rconst = 8.314 # Gas constant [J/(gmol.K)]

    ''' The first line MUST contain the number of components that will help to build up
        all numpy arrays thus, '''

    Are_There_Components = False

    ''' Open a file named 'input.dat' that contains all thermo-physical parameters for
        the model '''
    with open( 'input.dat', 'rt' ) as file:
        reader = csv.reader( file, delimiter = ' ', skipinitialspace = True )
        
        for row in reader:
            if row == [] or ListOfCommentsStrings( row ): # List of comment strings that can be used
                Nothing_To_Be_Done = True

            elif row[ 0 ] == 'Number_Components': # This MUST be the first variable declared in the input file
                Ncomp = int( row[ 1 ] )
                Are_There_Components = True

            elif Are_There_Components:              
    #
                if row[ 0 ] == 'Crit_Temp': # Critical temperature (in K)
                    T_Crit = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Crit_Pres': # Critical pressure (in Pa)
                    P_Crit = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Molar_Mass': # Molar mass (in g/gmol)
                    MolarMass = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Components':
                    Components = ReadingRows_String( row )
    #
                elif row[ 0 ] == 'Accentric_Factor':
                    Accentric_Factor = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Feed_Composition':
                    Z_Feed = ReadingRows_Float( row )
                    if ( abs( math.fsum( Z_Feed ) - 1. ) >= 1.e-5 ):
                        print 'Summation of Compositions is ', math.fsum( Z_Feed ), \
                            ' and it should be 1.0'
                        sys.exit()
    #
                elif row[ 0 ] == 'BinaryInteraction':
                    BinaryParameter = ReadingRows_FloatTensor( reader )
    #
            else:
                print 'Number_components was not defined in the FIRST line'
                sys.exit()
                
        print 'row:', Are_There_Components, Ncomp, T_Crit, P_Crit , MolarMass, Components,BinaryParameter




# This function reads a row containing float elements
def ReadingRows_Float( row ):
    Array = np.arange( float( Ncomp ) )
    for i in xrange( 0, Ncomp ):
        Array[ i ] = row[ i + 1 ]
    return Array

# This function reads a row containing characters elements
def ReadingRows_String( row ):
    Array = []
    Array.extend( row[ 1 : Ncomp + 1 ] )
    return Array

# This function reads a squared tensor of dimension ncomp X ncomp
def ReadingRows_FloatTensor( reader ):
    Array = np.arange( float( Ncomp * Ncomp ) )
    iline = 1
    for line in reader:
        for i in xrange( 0, Ncomp ):
            Array[ i ] = line[ i ]

    return Array
        

# This function assess if the summation of compositions (mole/mass fraction) is equal to one
def ListOfCommentsStrings( row ):
    list = []
    IsItaComment = False
    if (     row[ 0 ] == ''      or \
             row[ 0 ] == ' '     or \
             row[ 0 ] == '  '    or \
             row[ 0 ] == '   '   or \
             row[ 0 ] == ''      or \
             row[ 0 ] == '#'     or \
             row[ 0 ] == '##'    or \
             row[ 0 ] == '###'   or \
             row[ 0 ] == '# '    or \
             row[ 0 ] == '## '   or \
             row[ 0 ] == '### '  or \
             row[ 0 ] == '#  '   or \
             row[ 0 ] == '##  '  or \
             row[ 0 ] == '###  ' ):
        IsItaComment = True

    return IsItaComment
    


def set_Global_Variables2(): # Global variables
# Building up a small data bank for species C1 and C2:
    global Rconst, Nc, species, MW, Tc, Pc, w, FeedMoleFraction, kij

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



def set_Global_Variables(): # Global variables
# Building up a small data bank for species:
    global Rconst, Nc, species, MW, Tc, Pc, w, FeedMoleFraction, kij

    Rconst = 8.314 # Gas constant [J/(gmol.K)]

    Nc = 4 # Number of chemical species

    species = np.array( [ 'C1', 'C2', 'C3', 'C4' ] ) # List of chemical species

    MW = np.array( [ 16.043, 30.070, 44.097, 58.123 ] ) # Molar mass (in g/gmol)

    Tc = np.array( [ 190.4, 305.4, 369.8, 425.1 ] ) # Critical temperature (in K)

    Pc = np.array( [ 45.99, 48.72, 42.48, 37.96] ) # Critical pressure (in bar)
    Pc = Pc * 1.e5 # (converting into Pa)

    w = np.array( [ 0.012, 0.100, 0.152, 0.200 ] ) # Accentric factor

    FeedMoleFraction = np.array( [ 0.70, 0.15, 0.10, 0.05 ] ) # Feeding mole fraction

# Binary Interaction Parameter: this is stored as a tensor with indices 'i'and 'j': k(i,j). Thus for 4 components:

#    k11  k12  k13 k14      x      node1    node2    node3
#    k21  k22  k23 k24     node4     x      node5    node6
#    k31  k32  k33 k34     node7   node8     x       node9
#    k41  k42  k43 k44     node10  node11   node12     x

# with k(i,i) = k(j,j) = 0. and k(i,j) = k(j,i). The tensor (or matrix in this case) is stored as a simple array k( node )
# with coordinates: node = i * Nc + j 
# Therefore we just need to detrmine k(i,j). For 2 components
#    k11  k12
#    k21  k22
# with k11 = k22 = 0. and

    kij = [0. for i in range( Nc**2 ) ]
    for i in range( Nc ): 
        for j in range( Nc ):
            node = i * Nc + j
            if i == j: # Populating the main diagonal
                kij[ node ] = 0.
            elif i < j: # Populating the upper triangular matrix
                if i == 0:
                    if j == 1:
                         kij[ node ] = -0.003
                    elif j == 2:
                         kij[ node ] = 0.016
                    elif j == 3:
                         kij[ node ] = 0.019
                elif i == 1:
                    if j == 2:
                        kij[ node ] = 0.001
                    if j == 3:
                        kij[ node ] = 0.01
                elif i == 2:
                    if j == 3:
                        kij[ node ] = 0.003
                        
            else: # Populating the lower triangular matrix
                jj = i; ii = j ; node2 = ii * Nc + jj # with Kij = Kji, node2 returns the address of the node ij
                kij[ node ] = kij[ node2 ]

