#!/usr/bin/env python

import os, sys
import math
import numpy as np
import ThermoTools as ThT
import GibbsFunction as GibbsF
import PhaseStability as PhSty
import NormalisedConstraint as NC
import pickle
#import csv # Using csv (comma separated values) module


""" ======================================================================================
       This is the main function that calculates the fluid phase equilibria composition
           of a mixture containing "NComp" components. The thermodynamic formulation can
           be found in the documentation.
     ===================================================================================== """

def WrapThermodynamics( XSolution, **kwargs ):

    if kwargs:
        for key in kwargs:
            if key == 'Status':
                ProbStatus = kwargs[ key ]
            else:
                sys.exit('In WrapThermodynamics, option for problem-type was not defined')

    if ProbStatus == 'InitialCalculations':
        
        """  Reading Input data from external file """
        ThT.ReadSet_Global_Variables()
        if ThT.NPhase > 2:
            sys.exit( 'Most of the functions within were designed for a 2-phase systems. For further number of phases, the code will need to be ammended.' )

    else:
        sys.exit('Need code here ...')



    Temp = ThT.T_System[ 0 ]
    Press = ThT.P_System[ 0 ] 

    """                          Michaelsen Stability Criteria                                    """
    ( Composition, Comp_Phase, index_phase, Molar_Gibbs_Free ) = PhSty.Phase_Stability( Temp, Press )

    # Checking existing phases:
    PhSty.CheckingPhases( Comp_Phase, Molar_Gibbs_Free )

        
    return ( Molar_Gibbs_Free, Comp_Phase )


        
