
#!/usr/bin/env python

import numpy as np
import math 
import sys 
import ThermoTools as ThT

'''
    THESE FUNCTIONS CONVERT:
         (a) MOLE FRACTION INTO MASS FRACTION (Mole2Mass);
         (b) MASS FRACTION INTO MOLE FRACTION (Mass2Mole).
'''

def Mole2Mass( MolFrac ):
    ''' Given an array containing the mole fraction composition, this
           function returns the associated mass fraction composition
           using the molar mass of the input file.
    '''
    SumMolFrac = math.fsum( MolFrac * ThT.MolarMass )

    MassFrac = [ 0. for i in range( ThT.NComp ) ]
    for icomp in range( ThT.NComp ):
        MassFrac[ icomp ] = MolFrac[ icomp ] * ThT.MolarMass[ icomp ] / SumMolFrac

    Sum2One( 'Mass Fraction', MassFrac )

    return MassFrac
        

def Mass2Mole( MassFrac ):
    ''' Given an array containing the mass fraction composition, this
           function returns the associated mole fraction composition
           using the molar mass of the input file.
    '''
    MoleFrac = [ 0. for i in range( ThT.NComp ) ]
    
    if ThT.NComp == 2:
        '''
           If it is a binary system we do not need to rely on numerical solution.
        '''
        MoleFrac[ 0 ] = - MassFrac[ 0 ] * ThT.MolarMass[ 1 ] / \
            ( MassFrac[ 0 ] * ( ThT.MolarMass[ 0 ] - ThT.MolarMass[ 1 ] ) - \
                  ThT.MolarMass[ 0 ] )
        MoleFrac[ 1 ] = 1. - MoleFrac[ 0 ]

    else:
        ''' Solving system of linear equations. This may need to be changed/optimised \
               later on.
        '''
        A = [ [0. for j in range( ThT.NComp - 1 )] for i in range( ThT.NComp - 1 )]
        b = [ 0. for i in range( ThT.NComp - 1 ) ]
        for i in range( ThT.NComp - 1 ):
            for j in range( ThT.NComp - 1 ):
                if i == j:
                    A[ i ][ j ] = ThT.MolarMass[ i ] * ( MassFrac[ i ]  - 1. ) - \
                        MassFrac[ i ] * ThT.MolarMass[ ThT.NComp - 1 ]
                else:
                    A[ i ][ j ] = MassFrac[ i ] * ThT.MolarMass[ j ] - \
                        MassFrac[ i ] * ThT.MolarMass[ ThT.NComp - 1 ]

            b[ i ] = - MassFrac[ i ] * ThT.MolarMass[ ThT.NComp - 1 ]

        temp = solve( A, b )
        for i in range( ThT.NComp - 1 ):
            MoleFrac[ i ] = temp[ i ]
        MoleFrac[ ThT.NComp - 1 ] = 1. - math.fsum( temp )

    return MoleFrac
