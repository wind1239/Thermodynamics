
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import PhaseStability as Michaelsen
import MixingRules as MixRules


def GibbsObjectiveFunction( InitialAssessment, Temp, Press, Comp_Phase ):
    """ Comp_Phase array (dimension Nc X 1) contains:
            1. Nc-1 mole fractions and;
            2. 1 Phase mole fraction.
        The function returns an array with dimension Np containing the
            Chemical Potential for each component.
                                                                       """
    if ThT.NPhase > 2:
        print 'This function was hacked to work only on a 2-phases system, it thus needs to be generalised.'
        sys.exit()

    Alpha_Phase = Michaelsen.AlphaPhases() # Phase distribution
    MolFrac = [ 0. for i in range( ThT.NComp ) ]

    if InitialAssessment:
        if Alpha_Phase[ 1 ] > Alpha_Phase[ 0 ]:
            iphase_in = 1 ; iphase_out = 0 # Liquid (input) --> Vapour (ouput) 
        else:
            iphase_in = 0 ; iphase_out = 1 # Vapour (input) --> Liquid (output)
            
        """ Here, we calculate the Chemical Potential of all species in
                  (iphase_out) phase based on the composition of the 
                  (iphase_in) phase.                                       """
        ChemPot1 = Calculating_ChemPot( Temp, Press, ThT.Z_Feed, iphase_in, iphase_out, optional = 'Feed' )
        ChemPot2 = Calculating_ChemPot( Temp, Press, Comp_Phase, iphase_in, iphase_out )

    else:
        if Alpha_Phase[ 1 ] > Alpha_Phase[ 0 ]:
            iphase_in_1 = 1 ; iphase_out_1 = 0 # Liquid (input) --> Vapour (ouput) 
            iphase_in_2 = 1 ; iphase_out_2 = 1 # Liquid (input) --> Liquid (ouput) 
        else:        
            iphase_in_1 = 0 ; iphase_out_1 = 1 # Vapour (input) --> Liquid (ouput) 
            iphase_in_2 = 0 ; iphase_out_2 = 0 # Vapour (input) --> Vapour (ouput)
            
        ChemPot1 = Calculating_ChemPot( Temp, Press, Comp_Phase, iphase_in_2, iphase_out_2 )
        ChemPot2 = Calculating_ChemPot( Temp, Press, Comp_Phase, iphase_in_1, iphase_out_1 )

    """ Now Calculating each term of the molar Gibbs free energy:
            g = \Summation_{i=1}^{Nc-1} LX_{i}^{L} \left[\left(\mu_{i}^{L}-\mu_{i}^{V}\right) +
                     - \left(\mu_{n_{c}}^{L}-\mu_{n_{c}}^{V}\right)\right] +
                     + L \left(\mu_{n_{c}}^{L}-\mu_{n_{c}}^{V}\right)  +
                     + \Summation_{i=1}^{n_{c}} z_{i}\mu_{i}^{V}
                                                                                                 """
    """ Calculating:
                      \left(\mu_{n_{c}}^{L}-\mu_{n_{c}}^{V}\right)
                                                                                 """
    DiffChemPot_N = ChemPot1[ ThT.NComp - 1 ] - ChemPot2[ ThT.NComp - 1 ]


    """ Calculating:
                     Sum2 = \Summation_{i=1}^{Nc-1} x_{i}^{L} \left[\left(\mu_{i}^{L}-
                               \mu_{i}^{V}\right) - \left(\mu_{n_{c}}^{L}-
                               \mu_{n_{c}}^{V}\right)\right]
                                                                                       """
    sum1 = 0.
    for icomp in range( ThT.NComp - 1 ):
        MolFrac[ icomp ] = Comp_Phase[ icomp ]
        sum1 = sum1 + Comp_Phase[ icomp ]
    MolFrac[ ThT.NComp - 1 ] = 1. - sum1

    sum2 = 0.
    for icomp in range( ThT.NComp - 1 ):
        sum2 = sum2 + MolFrac[ icomp ] * ( ( ChemPot1[ icomp ] - ChemPot2[ icomp ] ) - \
                                           DiffChemPot_N )


    """ Calculating:
                     LSum2 = L * Sum2
                                                                              """
    LSum2 = MolFrac[ ThT.NComp - 1 ] * sum2

    """ Calculating:
                     Sum_ZMuV = \Summation_{i=1}^{n_{c}} z_{i}\mu_{i}^{V}
                                                                              """
    Sum_ZMuV = 0.
    for icomp in range( ThT.NComp ):
        Sum_ZMuV = Sum_ZMuV + ThT.Z_Feed[ icomp ] * ChemPot2[ icomp ]

    """ Calculating:
                     LMuNc =  L\left(\mu_{n_{c}}^{L}-\mu_{n_{c}}^{V}\right)
                                                                              """
    LMuNc = MolFrac[ ThT.NComp - 1 ] * DiffChemPot_N


    """ Adding all terms:
                          """
    GibbsEnergyMolar = LSum2 + LMuNc + Sum_ZMuV

    
    return GibbsEnergyMolar


#================
#
#================

def Calculating_ChemPot( Temp, Press, Comp_Phase, iphase_in, iphase_out, *positional_parameters, **keyword_parameters ):

    MolFrac = [ 0. for i in range( ThT.NComp ) ]
    if 'optional' in keyword_parameters:
        if keyword_parameters[ 'optional' ] == 'Feed':
            for icomp in range( ThT.NComp ):
                MolFrac[ icomp ] = Comp_Phase[ icomp ]

        else:
            print 'Unknown Optional parameter in Function Calculating_ChemPot'
            sys.exit()
            
    else:
        if iphase_in == iphase_out:
            sum1 = 0.
            for icomp in range( ThT.NComp - 1 ):
                MolFrac[ icomp ] = Comp_Phase[ icomp ]
                sum1 = sum1 + Comp_Phase[ icomp ]
            MolFrac[ ThT.NComp - 1 ] = 1. - sum1
            ThT.Sum2One( 'Mol Fraction', MolFrac ) # Ensuring the sum is equal to one and 
                                                   #     that there is no null component.   

        else:
            sum1 = 0. ; Phase = Comp_Phase[ ThT.NComp - 1 ]
            for icomp in range( ThT.NComp - 1 ):
                MolFrac[ icomp ] = ( ThT.Z_Feed[ icomp ] - Comp_Phase[ icomp ] * Phase ) / ( 1. - Phase )
                sum1 = sum1 + MolFrac[ icomp ]
            MolFrac[ ThT.NComp - 1 ] = 1. - sum1
            ThT.Sum2One( 'Mol Fraction', MolFrac ) # Ensuring the sum is equal to one and
                                                   #     that there is no null component.   

    ( FugCoef, ChemPot ) = MixRules.MixingRules_EoS( Temp, Press, iphase_out, MolFrac )


    return ChemPot

#================
#
#================

def CalcOtherPhase( Comp, Phase ):

    if ThT.NPhase > 2:
        print 'This function was hacked to work only on a 2-phases system, it thus needs to be generalised.'
        sys.exit()
    
    CompOtherPhase = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
    for iphase in range( ThT.NPhase - 1 ):
        for icomp in range( ThT.NComp ):
            node = iphase * ThT.NComp + icomp
            CompOtherPhase[ node ] = Comp[ node ]

    if abs( 1. - Phase ) >= ThT.Residual or abs( 1. - Comp[ 0 ] ) >= ThT.Residual :
        iphase = 1 ;  sum = 0.
        for icomp in range( ThT.NComp - 1 ):
            node0 = ( iphase - 1 ) * ThT.NComp + icomp ; node1 = iphase * ThT.NComp + icomp 
            if ( ThT.Z_Feed[ icomp ] - Phase * Comp[ node0 ] < 0. ):
                CompOtherPhase[ node1 ] = 0.
            else:
                CompOtherPhase[ node1 ] = ( ThT.Z_Feed[ icomp ] - Phase * Comp[ node0 ] ) / \
                    ( 1. - Phase )
                sum = sum + CompOtherPhase[ node1 ]
        CompOtherPhase[ iphase * ThT.NComp + ThT.NComp - 1 ] = 1. - sum

    return CompOtherPhase
        
                                                   
    



    
