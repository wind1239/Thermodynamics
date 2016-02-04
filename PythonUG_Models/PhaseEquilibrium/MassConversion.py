
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
        dsdas


        

''' This function solves a linear system:
           A . x = b
       using the Gauss-Seidl method with initial
       guess of
          x0 = [ 0.5 0.5 ... 0.5 ]
'''
def GaussSeidl( n, A, b ):

    Guess = [ 0.50 for i in range( n ) ]
    NIter_max = 501 # Stoppage criteria: Max number of iterations
    Eps = 1.e-5 # Stoppage criteria: Max error allowed

    iter = 1
    while iter <= NIter_max:

        for i in range( n ):
            for j in range( n ):

                sum1 = 0. ; sum2 = 0.
                if j < i:
                    sum1 = sum1 + A[



        iter += 1

    

    
        
