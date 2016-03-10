
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import MixingRules as MixRules


def Phase_Stability( Temp, Press ):

    ''' ==============================================================
          Let's choose an initial configuration (i.e., composition)
            for the free Gibbs energy of liquid and vapour phases
        ==============================================================
    '''
    Alpha = AlphaPhases()
    if ThT.Debug:
        print 'Alpha ( V / L ):', Alpha

    # Find the maximum Alpha that indicates the choice of the phase:
    BigNumber = - sys.float_info.max
    for iphase in range( ThT.NPhase ):
        if Alpha[ iphase ] > BigNumber:
            BigNumber = Alpha[ iphase ] ; index_phase = iphase

    if ThT.Debug:
        print ' '
        print '================================================='
        print '       Initial stability test. Phase is:'
        if index_phase == 0:
            print '    ---->        Vapour        <----     '
        elif index_phase == 1:
            print '    ---->        Liquid        <----    '
        else:
            sys.exit()
        print ' '
        print '================================================='

    """ Calculating the Fugacity coefficient and Gibbs free energy (i.e., chemical potential) of
            the chosen phase (index_phase) """
    ( Fug_Coeff, G_Phase ) = MixRules.MixingRules_EoS( Temp, Press, index_phase, ThT.Z_Feed )
    
    Composition = [ 0. for i in range( ThT.NComp ) ]
    for icomp in range( ThT.NComp ):
        if icomp == ( ThT.NComp - 1 ):
            Composition[ icomp ] = ThT.PhaseFrac[ index_phase ]
        else:
            Composition[ icomp ] = ThT.MFrac[ index_phase * ThT.NComp + icomp ]

    GZero = 0.
    for icomp in range( ThT.NComp ):
        GZero = GZero + ThT.Z_Feed[ icomp ] * G_Phase[ icomp ]
        
    return GZero

#==============
#
#==============

def AlphaPhases():
    '''
       This function returns Alpha(j) = SUM( Z(i) - X(i,j)),
       where i = 1, NComp and j = 1, Nphase.

    '''
    Alpha_Phase = [ 0. for i in range( ThT.NPhase ) ]
    for iphase in range( ThT.NPhase ):
        for icomp in range( ThT.NComp ):
            node = iphase * ThT.NComp + icomp
            Alpha_Phase[ iphase ] = Alpha_Phase[ iphase ] + ( ThT.Z_Feed[ icomp ]  - ThT.MFrac[ node ] )**2
        Alpha_Phase[ iphase ] = math.sqrt( Alpha_Phase[ iphase ] )
        
    return Alpha_Phase

        

