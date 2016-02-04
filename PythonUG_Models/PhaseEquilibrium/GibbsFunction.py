
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS as EoS


def Calc_Gibbs( Temp, Press ):
    GibbsEnergy = sys.float_info.max

    ''' ==============================================================
    
          LET'S CHOOSE AN INITIAL CONFIGURATION (i.e., COMPOSITION)
             FOR FREE GIBBS ENERGY FOR LIQUID AND VAPOUR PHASES

        ==============================================================  '''

    Alpha_Phase = [ 0. for i in range( ThT.NPhase ) ]
    for iphase in range( ThT.NPhase ):
        for icomp in range( ThT.NComp ):
            node = iphase * ThT.NComp + icomp
            Alpha_Phase[ iphase ] = Alpha_Phase[ iphase ] + ( ThT.Z_Feed[ icomp ] - ThT.MFrac[ node ] )**2

        Alpha_Phase[ iphase ] = math.sqrt( Alpha_Phase[ iphase ] )


    ''' This loop needs to be changed into take into account an
                     arbitrary number of phases. '''

    G1 = [ 0. for i in range( ThT.NComp ) ]
    for iphase in range( ThT.NPhase ): 
        for jphase in range( iphase + 1, ThT.NPhase ):
            if Alpha_Phase[ jphase ] > Alpha_Phase[ iphase ]:
                print 'Chemical Potential Liquid' # FunctionChemPot_Liq
                Test_Composition = ThT.MFrac[ jphase * ThT.NComp : jphase * ThT.NComp + ThT.NComp ]
                Test_Composition[ ThT.NComp - 1 ] = ThT.PhaseFrac[ jphase ]
                G1 = 1. # = FunctionChemPot_Liq
            else:
                print 'Chemical Potential Vapour' # FunctionChemPot_Vap
                Test_Composition = ThT.MFrac[ iphase * ThT.NComp : iphase * ThT.NComp + ThT.NComp ]
                Test_Composition[ ThT.NComp - 1 ] = ThT.PhaseFrac[ iphase ]
                G1 = 1. # = FunctionChemPot_Vap

    GZero = math.fsum( ThT.Z_Feed * G1 )

    
                
                

    for i in range( ThT.NComp ):
        print 'here1:', ThT.Species[ i ], EoS.Cubic_EoS( i, Temp )

    ''' Do something here ... '''

    return GibbsEnergy
    



    
