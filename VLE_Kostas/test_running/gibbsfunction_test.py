#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import calculate_EOS_PR_test as PR
import calculate_ln_gamma_test as lng
import calculate_fi_test as fi
import calculate_terms_test as terms
import calculate_chemical_potential_test as chemp
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter
import matplotlib.pyplot as plt
import time 

###
### Functions Call_Calc_ChemPot
###
def Call_Calc_ChemPot( MFrac ):  # This function will ...

    for iphase in range(ThT.NPhase): # Loop for Phases
        node_init = iphase * ThT.NComp ; node_final = iphase * ThT.NComp + ThT.NComp
        ChemPot = [0. for z in range(ThT.NComp * ThT.NPhase) ] # Creating a temporary array ChemPot

        print MFrac[ node_init:node_final ], node_init, node_final
        ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )

    # End Loop for Phases

        ''' NOT SURE WHY O YOU NEED i AND j here ... BOTH CONDITIONALS DO THE SAME THING
                if i == j: # What am I doing here ...  What is the diff here in this conditional ???
                    print MFrac[ node_init:node_final ], node_init, node_final
                    ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )
                else:
                    print '  i am in the else case ' #' for the component ', ThT.Species[i],' with respect to' , ThT.Species[j]
                    ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )
                # end of the  i=j conditional representing ... '''

    return ChemPot

###
### Function Call_Calc_Gibbs:
###
def Call_Calc_Gibbs( MFrac, ChemPot ): # This function will ...
    
    """ Setting molar fraction of the phases in equilibrium """
    PhaseFrac = [0. for k in range( ThT.NPhase ) ] #; PhaseFrac[ 0 ] = 0.35 ;  PhaseFrac[ 1 ] = 1. - PhaseFrac[ 0 ]

    '''This needs to be generalised later on ... for now it is just a quick hack ... '''
    Temp = MFrac[ 0 ] - MFrac[ 2 ]
    if ( abs( Temp - 0. ) <= 1.e-10 ):
        sys.exit()
    else:
        PhaseFrac[ 0 ] = ( ThT.Z_Feed[0] - MFrac[ 2 ] ) / Temp ; PhaseFrac[ 1 ] = 1. - PhaseFrac[ 0 ]  
        # the line above describes the condition for the feed composition zi = V * yi + (1 - V) * xi
    
    sumGibbs = 0. ; sumfeedk = 0.
    for icomp in range( ThT.NComp ): # Loop ICOMP
        Vphase = 0 ; Lphase = 1 
        nodeV = Vphase * ThT.NComp + icomp ; nodeL = Lphase * ThT.NComp + icomp             # a clever way to describe the nodes for Vapour and liquid phases
        nodeVfinal = Vphase * ThT.NComp + ThT.NComp - 1; nodeLfinal = Lphase * ThT.NComp + ThT.NComp - 1
        
        if icomp <= ThT.NComp - 2: # Conditional ICOMP
            sumGibbs = sumGibbs + PhaseFrac[ Lphase ] * MFrac[ nodeL ]  * ( ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) - ( ChemPot[ nodeLfinal ]  - ChemPot[ nodeVfinal ] ) )

        # End of Conditional ICOMP
        
        sumfeedk = sumfeedk + ThT.Z_Feed[icomp] * ChemPot[ nodeV ]

    # End of Loop ICOMP
    Gibbs = sumGibbs + sumfeedk

    return Gibbs


####

""" THIS IS THE MAIN FUNCTION FOR THE GIBBS FREE ENERGY CALCULATION """

def GibbsCalculation( XSolution ):


#    print
#    print
#    print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
#    print
#    print


    ThT.ReadSet_Global_Variables()                             # reading the external file

    """ Populating the MFRAC array that will be used throughout the calculation. In this thermodynamic module,
           MFRAC contains the mole fraction of all components in the vapour (0:Nc-1) and liquid (Nc:Nc*Np)
           phases. This function has XSolution[0:Nc] as an input and it contains the Nc-1 mole fractions
           of copmponents at the liquid phase and molar fraction of liquid phase.                             """

    MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]    # create an a array with 0 values for the MFrac

    Lphase = XSolution[ ThT.NComp - 1 ] ;  Vphase = 1. - Lphase ; Sum1 = 0. ;  inc = 0

    for icomp in xrange( ThT.NComp, ThT.NComp * ThT.NPhase ): # Determining MFrac @ Liquid Phase
        if icomp < ThT.NComp * ThT.NPhase - 1:
            MFrac[ icomp ] = XSolution[ inc ]
            Sum1 = Sum1 + MFrac[ icomp ]
            inc += 1

    MFrac[ ThT.NComp * ThT.NPhase - 1 ] = 1. - Sum1 # Determining MFrac @ Liquid Phase (last component)

    Sum2 = 0.
    for icomp in range( ThT.NComp - 1 ): # Determining MFrac @ Vapour phase
        MFrac[ icomp ] = ( ThT.Z_Feed[ icomp ] - Lphase * MFrac[ ThT.NComp + icomp ] ) / Vphase
        Sum2 = Sum2 + MFrac[ icomp ]

    MFrac[ ThT.NComp - 1 ] = 1. - Sum2 # Determining MFrac @ Vapour Phase (last component)

    ChemPot = Call_Calc_ChemPot( MFrac ) # ChemPot has dimension (NPhase * NComp)

    #MolarGibbs = -Call_Calc_Gibbs( MFrac, ChemPot ) 
    MolarGibbs = Call_Calc_Gibbs( MFrac, ChemPot ) 


    return MolarGibbs, ThT.Z_Feed





          
            
         
           
  

          
