
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import GibbsFunction as GibbsFcn


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
            BigNumber = Alpha[ iphase ] ;index_phase = iphase


    G_Phase = GibbsFcn.GibbsPhase( Temp, Press, iphase )


    return

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

        

