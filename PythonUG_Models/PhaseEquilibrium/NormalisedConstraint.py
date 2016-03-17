#!/usr/bin/env python

import numpy as np
import math
import ThermoTools as ThT
     
                                                   
#================
#
#================

def CalcOtherPhase( Array, iphase, *args, **kwargs ):
    '''
       Input:
               Array: an array containing composition
               Length (opt): (a) MoleFrac_Only      or MassFrac_Only
                             (b) MolePhaseFrac_Only or MassPhaseFrac_Only
                             (c) MolePhaseFrac      or MassPhaseFrac
               PhaseFrac (opt): If (a), then a array PhaseFrac (containing molar/mass
                                  fractions of the phases) needs to be included
                                    
       Output: 
               MFrac_OtherPhase is an array of length NComp X NPhase containing ONLY
                    mole/mass fraction of each component at each phase.
               PFrac_OtherPhase is an array of length NPhase containing only the phase
                    molar/mass fraction of each phase.                                     
                                                                                        '''
    
    MFrac_OtherPhase = [ 0. for i in range( ThT.NComp ) ]
    NearlyOne = 1. - ThT.Residual

    sumphase1 = 0.
    for jphase in range( ThT.NPhase ):
        node_ph = jphase * ThT.NComp + ThT.NComp - 1
        if jphase != iphase:
            sumphase1 = sumphase1 + Array[ node_ph ]

    sumcomp = 0.
    for icomp in range( ThT.NComp - 1 ):
        sumphase2 = 0.
        for jphase in range( ThT.NPhase ):
            node_ph = jphase * ThT.NComp + ThT.NComp - 1
            node_comp = jphase * ThT.NComp + icomp
            if jphase |= iphase:
                sumphase2 = sumphase2 + Array[ node_ph ] * Array[ node_comp ]

        MFrac_OtherPhase[ icomp ] = max( ThT.Residual, ( ThT.Z_Feed[ icomp ] - sumphase2 ) ) /\
                                    min( NearlyOne, max( ThT.Residual, sumphase1 ) )
        sumcomp = sumcomp + MFrac_OtherPhase[ icomp ]

    MFrac_OtherPhase[ ThT.NComp - 1 ] = min( NearlyOne, max( ThT.Residual, 1. - sumcomp ) )

    s
    
        

    

    if 'Length' in  kwargs:
        length = kwargs.get( 'Length', None )
        if length == 'MoleFrac_Only' or length == 'MassFrac_Only':
            """ Array contains only the mole/mass fraction for ONE phase """
            Array2 = kwargs.get( 'PhaseFrac' )
            temp = OtherPhase( Array, PhaseFrac = Array2 )


        elif length == 'MolePhaseFrac_Only' or length == 'MassPhaseFrac_Only':
            """ Array contains (NComp - 1) mole/mass fraction + phase molar/mass fraction for ONE phase """
            temp = OtherPhase( Array )
            temp[ ThT.NComp - 1 ] = Array[ ThT.NComp - 1 ]
            

        elif length == 'MolePhaseFrac' or length == 'MassPhaseFrac':
            """ Array contains mole/mass fracion + phase molar/mass fraction for ALL phases """
            temp = OtherPhase( Array, Number_Phases = ThT.NPhase )
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

def OtherPhase( Array, *args, **kwargs ):

    if 'PhaseFrac' in kwargs: # Array contains just mole/mass fraction of ONE phase
        PhaseFrac = kwargs.get( 'PhaseFrac', None )

    ndim = np.shape( Array )[ 0 ]
    MFrac_OtherPhase =  [ 0. for i in range( ndim1 ) ]
    if ndim1 == ThT.NPhase * ThT.NComp: # Array contains all phases
        PFrac_OtherPhase = [ 0. for i in range( ThT.NPhase ) ]
    else:
        if 'PhaseFrac' in kwargs: # Array contains just mole/mass fraction of ONE phase
            Array2 = kwargs.get( 'PhaseFrac', None )
            ndim2 = np.shape( Array2 )[ 0 ]
            MFrac_OtherPhase = [ 0. for i in range( ndim1 ) ]
            PFrac_OtherPhase = [ 0. for i in range( ndim2 ) ]
        else: # Array contains mole/mass fraction + phase fraction of ONE phase
    MFrac_OtherPhase = [ 0. for i in range( ndim1 ) ]
    






    MFrac_OtherPhase = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
    PFrac_OtherPhase = [ 0. for i in range( ThT.NPhase ) ]
    NearlyOne = 1. - ThT.Residual

    for iphase in range( ThT.NPhase ): # Creating MFrac/PFrac_OtherPhase arrays based on Frac
        sumcomp = 0.
        for icomp in range( ThT.NComp ):
            node = iphase * ThT.NComp + icomp
            if icomp != ThT.NComp - 1 :
                MFrac_OtherPhase[ node ] = Frac[ node ]
                sumcomp = sumcomp + MFrac_OtherPhase[ node ]
            else:
                MFrac_OtherPhase[ node ] = 1. - sumcomp
                PFrac_OtherPhase[ iphase ] = Frac[ node ]

    kphase = Phase
    for icomp in range( ThT.NComp ):
        node_kphase = kphase * ThT.NComp + icomp
        sumcomp = 0. ; sumphase = 0.
        for iphase in range( ThT.NPhase ):
            node_iphase = iphase * ThT.NComp + icomp
            if iphase != kphase:
                sumcomp = sumcomp + PFrac_OtherPhase[ iphase ] * MFrac_OtherPhase[ node_iphase ]
                sumphase = sumphase + PFrac_OtherPhase[ iphase ]
                
        MFrac_OtherPhase[ node_kphase ] = min( NearlyOne, max( ThT.Residual, ThT.Z_Feed[ icomp ] - sumcomp ) / max( ThT.Residual, 1. - sumphase ) )

    if 'Length' in kwargs:
        length = kwargs.get( 'Length', None )
        if length == 'short': # This returns the mole/mass fraction of phase 'Phase'
            Frac_OtherPhase = [ 0. for i in range( ThT.NComp ) ]
            for icomp in range( ThT.NComp ):
                node_kphase = kphase * ThT.NComp + icomp
                Frac_OtherPhase[ icomp ] = MFrac_OtherPhase[ node_kphase ]
            return Frac_OtherPhase

        elif length == 'long': # This returns the mole/mass fraction of all phases
            return MFrac_OtherPhase
            Frac_OtherPhase = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
            for iphase in range( ThT.NPhase ):
                for icomp in range( ThT.NComp ):
                    node = iphase * ThT.NComp + icomp
                    
                    

    else:
        return ( MFrac_OtherPhase, PFrac_OtherPhase )
        #Frac_OtherPhase = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
        #Frac_OtherPhase = MFrac_OtherPhase

        

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
            """ Array contains (NComp - 1) mole/mass fraction + phase molar/mass fraction for ONE phase """
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

    ndim = np.shape( Array )[ 0 ]
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

