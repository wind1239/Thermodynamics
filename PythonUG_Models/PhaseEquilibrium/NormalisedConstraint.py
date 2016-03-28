#!/usr/bin/env python

import numpy as np
import math
import ThermoTools as ThT
     
                                                   
#================
#
#================

def CalcOtherPhase( Array, kphase ):#, *args, **kwargs ):
    '''
       This function calculates the composition of phase [kphase] as
           X_{i}^{k} = ( Z_{i} - Summation_{j=1,j!/k}^{NPhase} \Pi^{j}X_{i}^{j} ) /
                  ( 1 - Summation_{j=1,j!/k}^{NPhase} \Pi^{j} )
           Input:
                   Array: an array containing [ NPhase*(NComp-1) ] mole/mass fraction of
                             individual components and (NPhase) molar/mass fraction of
                             individual phases ( NPhase x NComp );
                   kphase: Phase in hich the components that we want to calculate are
                             contained;
           Output: 
                   MFrac_OtherPhase is an array of length NComp containing ONLY the
                              mole/mass fraction of each component at each phase.
                   MFrac is an array of length NComp * NPhase containing ONLY the
                              mole/mass fraction of each component in all phases. 
                                                                                        '''
    
    MFrac_OtherPhase = [ 0. for i in range( ThT.NComp ) ]
    MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
    NearlyOne = 1. - ThT.Residual

    sumphase1 = 0.
    for jphase in range( ThT.NPhase ):
        node_ph = jphase * ThT.NComp + ThT.NComp - 1
        if jphase != kphase:
            sumphase1 = sumphase1 + Array[ node_ph ]

    sumcomp = 0.
    for icomp in range( ThT.NComp - 1 ):
        sumphase2 = 0.
        for jphase in range( ThT.NPhase ):
            node_ph = jphase * ThT.NComp + ThT.NComp - 1
            node_comp = jphase * ThT.NComp + icomp
            if jphase != kphase:
                sumphase2 = sumphase2 + Array[ node_ph ] * Array[ node_comp ]

        MFrac_OtherPhase[ icomp ] = min( NearlyOne, max( ThT.Residual, max( ThT.Residual, ( ThT.Z_Feed[ icomp ] - sumphase2 ) ) /\
                                                         min( NearlyOne, max( ThT.Residual, sumphase1 ) ) ) )
        sumcomp = sumcomp + MFrac_OtherPhase[ icomp ]

    MFrac_OtherPhase[ ThT.NComp - 1 ] = min( NearlyOne, max( ThT.Residual, 1. - sumphase1 ) )

    for iphase in range( ThT.NPhase ):
        for icomp in range( ThT.NComp ):
            node = iphase * ThT.NComp + icomp
            if iphase != kphase:
                MFrac[ node ] = Array[ node ]
            else:
                MFrac[ node ] = MFrac_OtherPhase[ icomp ]

    return MFrac
        

#=================================
#
#=================================
'''
    The following functions ensure that the sum of an array is within the interval 
        (0., 1.). Note that this interval excludes 0. and 1.
                                                              '''
def SetBox( Array,  *args, **kwargs ):

    NearlyOne = 1. - ThT.Residual

    if 'Length' in kwargs:
        length = kwargs.get( 'Length', None )
        if length == 'MoleFrac_Only' or length == 'MassFrac_Only':
            """ Array contains only the mole/mass fraction for ONE phase """
            temp = BoxSummation( Array )


        elif length == 'MolePhaseFrac_Only' or length == 'MassPhaseFrac_Only':
            """ Output: Array contains (NComp - 1) mole/mass fraction +
                        phase molar/mass fraction for ONE phase            """
            temp = BoxSummation( Array )
            temp[ ThT.NComp - 1 ] = Array[ ThT.NComp - 1 ]
            

        elif length == 'MolePhaseFrac' or length == 'MassPhaseFrac':
            """ Array contains mole/mass fracion + phase molar/mass fraction for ALL phases """
            temp = BoxSummation( Array, Number_Phases = ThT.NPhase )
            for iphase in range( ThT.NPhase ):
                node = iphase * ThT.NComp + ThT.NComp - 1
                temp[ node ] = Array[ node ]

                
        else:
            print 'Wrong options/arguments for function Set_Composition_Box'
            sys.exit()

    else:
        print 'No option was included -re the format of Array'
        sys.exit()


    return temp

#=================================
#
#=================================
            
def BoxSummation( Array, *args, **kwargs ):

    ndim = np.shape( Array )[ 0 ] ; NearlyOne = 1. - ThT.Residual
    if 'Number_Phases' in kwargs:
        NPhase = kwargs.get( 'Number_Phases', None )
    else:
        NPhase = 1

    NComp = ndim / NPhase
    temp = [ 0. for i in range( ndim ) ]

    for iphase in range( NPhase ):
        sumcomp = 0.
        for icomp in range( NComp - 1 ):
            node = iphase * NComp + icomp
            if abs( Array[ node ] - 0. ) <= ThT.Residual or \
               Array[ node ] < 0.:
                temp[ node ] = ThT.Residual
            elif abs( Array[ icomp ] - 1. ) <= ThT.Residual or \
                 Array[ node ] > 1.:
                temp[ node ] = NearlyOne
            else:
                temp[ node ] = Array[ node ]

            sumcomp = sumcomp + temp[ node ]

        temp[ iphase * NComp + NComp - 1 ] = min( NearlyOne, max( ThT.Residual, 1. - sumcomp ) )

    return temp


#=======================================
#
#=======================================

def Generate_PhaseFraction( iter_phase, Inc_Phase ):
    """ PhaFrac has dimension NPhase """

    PhaFrac = [ 0. for i in range( ThT.NPhase ) ] ; NearlyOne = 1. - ThT.Residual

    sumphase = 0.
    for iphase in range( ThT.NPhase - 1 ):
        PhaFrac[ iphase ] = min( NearlyOne, max( ThT.Residual, float( iter_phase ) * Inc_Phase ) )
        sumphase = sumphase + PhaFrac[ iphase]
    PhaFrac[ ThT.NPhase - 1 ] = 1. - sumphase

    return PhaFrac


#=======================================
#
#=======================================

def Generate_MoleFraction( PhaFrac, iter_comp, Inc_Comp ):
    """ MolFrac has dimension NPhase * NComp """

    MolFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ] ; NearlyOne = 1. - ThT.Residual

    sum = 0.
    for iphase in range( ThT.NPhase - 1 ):
        for icomp in range( ThT.NComp - 1):
            node = iphase * ThT.NComp + icomp
            MolFrac[ node ] = min( NearlyOne, max( ThT.Residual, float( iter_comp ) * Inc_Comp ) )
            sum = sum + MolFrac[ node ]
        node2 = iphase * ThT.NComp + ThT.NComp - 1
        MolFrac[ node2 ] = PhaFrac[ iphase ]

    MolFrac = CalcOtherPhase( MolFrac, ThT.NPhase - 1 ) # Calculating the mole/mass fraction
                                                        # of the ThT.NPhase phase

    return MolFrac
    
