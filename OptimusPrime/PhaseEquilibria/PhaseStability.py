
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

        Output:
                 Composition is an array containing the mole/mass fraction of
                        all components in phase INDEX_PHASE;
                 Comp_Phase is an array that contains (NComp - 1) mole/mass
                        fraction + ( 1 ) molar/mass fraction of the phase;
                 GZero contains the molar Gibbs energy based on the feed mole/
                        mass fraction.
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
    Comp_Phase = [ 0. for i in range( ThT.NComp ) ]

    for icomp in range( ThT.NComp ):
        Composition[ icomp ] = ThT.MFrac[ index_phase * ThT.NComp + icomp ]
        if icomp == ThT.NComp - 1:
            Comp_Phase[ icomp ] = ThT.PhaseFrac[ index_phase ]
        else:
            Comp_Phase[ icomp ] = ThT.MFrac[ index_phase * ThT.NComp + icomp ]


    GZero = 0.
    for icomp in range( ThT.NComp ):
        GZero = GZero + ThT.Z_Feed[ icomp ] * G_Phase[ icomp ]
        
    return ( Composition, Comp_Phase, index_phase, GZero )

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
        sum = 0.
        for icomp in range( ThT.NComp ):
            node = iphase * ThT.NComp + icomp
            sum = sum + ( ThT.Z_Feed[ icomp ]  - ThT.MFrac[ node ] )**2
            
        Alpha_Phase[ iphase ] = math.sqrt( sum )
        
    return Alpha_Phase


#==============
#
#==============

def CheckingPhases( Comp_Phase, GibbsPhase ):
    """ Comp_Phase has dimension NComp. """

    """ Assessing the results of the VLE calculation and identifying the
        phase and compositions (mol fractions).
        Comp_Phase is an array that contains (NComp-1) mole fractions +
        (1) PhaseFraction                                                """

    if ThT.NPhase > 2:
        print 'This function was hacked to work only on a 2-phases system, it thus needs to be generalised.'
        sys.exit()

    Gibbs = -GibbsPhase
    YSum = 0.
    for icomp in range( ThT.NComp - 1):
        YSum = YSum + Comp_Phase[ icomp ]
    XMet = 1. - YSum


    if XMet > ThT.Z_Feed[ ThT.NComp - 1 ]: # Liquid phase        
        LiqPhase = Comp_Phase[ ThT.NComp - 1 ]
        VapPhase = 1. - LiqPhase

        if Comp_Phase[ ThT.NComp - 1 ] < 0.5:
            print 'Dominant Vapour Phase '
        else:
            print 'Dominant Liquid Phase '

        """  Results for the Vapour Phase """
        XVOpt = [ 0. for i in range( ThT.NComp ) ]
        XVOpt[ ThT.NComp - 1 ] = 1. - Comp_Phase[ ThT.NComp - 1 ] ; Sum = 0.
        for icomp in range( ThT.NComp - 1 ):
            XVOpt[ icomp ] = ThT.Z_Feed[ icomp ] - Comp_Phase[ icomp ] * Comp_Phase[ ThT.NComp - 1 ] / \
                XVOpt[ ThT.NComp - 1 ]
            Sum = Sum + XVOpt[ icomp ]
        XMetV = 1. - Sum

        """ Now calculating the mass of each component in this phase based 
            on the molar mass of the individual components.                 """
        MassVapour = 0. ; MassLiquid = 0.
        for icomp in range( ThT.NComp - 1 ):
            MassLiquid = MassLiquid + Comp_Phase[ icomp ] * ThT.MolarMass[ icomp ]
            MassVapour = MassVapour + XVOpt[ icomp ] * ThT.MolarMass[ icomp ]

        MassLiquid_Total = MassLiquid + XMet  * ThT.MolarMass[ ThT.NComp - 1 ]
        MassVapour_Total = MassVapour + XMetV * ThT.MolarMass[ ThT.NComp - 1 ]

        if MassLiquid_Total > MassVapour_Total:
            print ' '
            print '==================================================================='
            print ' '
            print ' Composition of', ThT.Species[0],':', Comp_Phase[ 0 ]
            print ' Liquid phase Composition:', LiqPhase
            print ' Gibbs free energy:', Gibbs
            print ' '
            print '==================================================================='
            print ' '

        else:
            print ' '
            print '==================================================================='
            print ' '
            print ' Composition of', ThT.Species[0],':', Comp_Phase[ 0 ]
            print ' Vapour phase Composition:', VapPhase
            print ' Gibbs free energy:', Gibbs
            print ' '
            print '==================================================================='
            print ' '

    else: # Vapour phase       
              
        VapPhase = Comp_Phase[ ThT.NComp - 1 ]
        LiqPhase = 1. - VapPhase

        if Comp_Phase[ ThT.NComp - 1 ] < 0.5:
            print 'Dominant Liquid Phase '
        else:
            print 'Dominant Vapour Phase '

        """  Results for the Liquid Phase """
        XLOpt = [ 0. for i in range( ThT.NComp ) ]
        XLOpt[ ThT.NComp - 1 ] = 1. - Comp_Phase[ ThT.NComp - 1 ] ; Sum = 0.
        for icomp in range( ThT.NComp - 1 ):
            XLOpt[ icomp ] = ThT.Z_Feed[ icomp ] - Comp_Phase[ icomp ] * Comp_Phase[ ThT.NComp - 1 ] / \
                XLOpt[ ThT.NComp - 1 ]
            Sum = Sum + XLOpt[ icomp ]
        XMetL = 1. - Sum

        """ Now calculating the mass of each component in this phase based 
            on the molar mass of the individual components.                 """
        MassVapour = 0. ; MassLiquid = 0.
        for icomp in range( ThT.NComp - 1 ):
            MassLiquid = MassLiquid + XLOpt[ icomp ] * ThT.MolarMass[ icomp ]
            MassVapour = MassVapour + Comp_Phase[ icomp ] * ThT.MolarMass[ icomp ]

        MassLiquid_Total = MassLiquid + XMetL * ThT.MolarMass[ ThT.NComp - 1 ]
        MassVapour_Total = MassVapour + XMet  * ThT.MolarMass[ ThT.NComp - 1 ]

        if MassLiquid_Total > MassVapour_Total:
            print ' '
            print '==================================================================='
            print ' '
            print ' Composition of', ThT.Species[0],':', Comp_Phase[ 0 ]
            print ' Liquid phase Composition:', LiqPhase
            print ' Gibbs free energy:', Gibbs
            print ' '
            print '==================================================================='
            print ' '

        else:
            print ' '
            print '==================================================================='
            print ' '
            print ' Composition of', ThT.Species[0],':', Comp_Phase[ 0 ]
            print ' Vapour phase Composition:', VapPhase
            print ' Gibbs free energy:', Gibbs
            print ' '
            print '==================================================================='
            print ' '


    return 
        



#==============
#
#==============

    
