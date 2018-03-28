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

        #OutFile = open('output', 'w')

        ''' ====================================================================
               Printing the data from the input file 
            ===================================================================='''

        print 'Temp of the mixture (initial, final, increment):', ThT.T_System
        print 'Pres of the mixture (initial, final, increment):', ThT.P_System
        #
        print 'Species: ', ThT.Species
        print 'Feed Composition: ', ThT.Z_Feed
        print 'Critical Temperature:', ThT.T_Crit
        print 'Critical Pressure:', ThT.P_Crit
        print 'Molar Mass:', ThT.MolarMass
        #
        print 'Binary Parameter:', ThT.BinaryParameter
        #
        print '******  Initial guess composition   *******'
        for i in range( ThT.NPhase ):
            print 'Phase:', i, ':', ThT.MFrac[ i * ThT.NComp : i * ThT.NComp + ThT.NComp ]

        print 'Composition of the phases:', ThT.PhaseFrac

        ''' ====================================================================
               Printing the data from the input file 
            ===================================================================='''

        ''' Initialising Temperature and Pressure '''
        Temp = ThT.T_System[ 0 ]
        Press = ThT.P_System[ 0 ] 



        if ThT.NPhase > 2:
            print 'This function was hacked to work only on a 2-phases system, it thus needs to be generalised.'
            sys.exit()

        else:
            
            # Preparing loop for automatically generating composition
            #ThT.MFrac = 0. ; ThT.PhaseFrac = 0.
            Inc_Phase = 0.25 ; Inc_Comp = 0.1
            NearlyOne = 1. - ThT.Residual

            Niter_Phase = int( 1. / Inc_Phase ) + 1 ; Niter_Comp = int( 1. / Inc_Comp ) + 1

            Molar_Gibbs_Free = [ 0. for i in range( Niter_Phase * Niter_Comp ) ]
            Composition  = [ 0. for i in range( Niter_Phase * Niter_Comp ) ] 
            Phase = [ 0. for i in range( Niter_Phase * Niter_Comp ) ]
            Comp_Phase = [ 0. for i in range( ThT.NComp ) ]


            #if False:

            """ This is a temporary hack to generate (on-the-fly) molar phase (mole/mass 
                     fraction): Vapour (PhaFrac[0]) and Liquid (PhaFrac[1]).             """
            for iter_phase in range( Niter_Phase ):
                ThT.PhaseFrac = NC.Generate_PhaseFraction( iter_phase, Inc_Phase )
                print 'PhaseFrac:', ThT.PhaseFrac ; sys.exit()


                """ This is a temporary hack to generate (on-the-fly) molar composition
                         of all phases.                                                  """
                for iter_comp in range( Niter_Comp ):
                    ThT.MFrac = NC.Generate_MoleFraction( iter_comp, Inc_Comp )

                    if ThT.Debug:
                        print ' ===================================='
                        print '    Composition of components in all phases: '
                        print '      ThT.MFrac:', ThT.MFrac
                        print ' '
                        print ' '
                        print '    Composition of both phases: '
                        print '      ThT.PhaseFrac:', ThT.PhaseFrac
                        print ' '
                        print ' ===================================='
                        print ' '

        """ Michaelsen Stability Criteria """
        ( Composition, Comp_Phase, index_phase, Molar_Gibbs_Free ) = PhSty.Phase_Stability( Temp, Press )
        PhSty.CheckingPhases( Comp_Phase, Molar_Gibbs_Free )
        #sys.exit('===')#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    else:
        sys.exit('missing code')
        
    return ( Molar_Gibbs_Free, Comp_Phase )


        
