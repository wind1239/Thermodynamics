
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math
import ThermoTools as ThT
import GibbsFunction as GibbsF
import PhaseStability as Michaelsen
import NormalisedConstraint as NC
import pickle
#import csv # Using csv (comma separated values) module


''' This is the main program that aims to calculate the vapour-liquid equilibrium
       composition of a mixture containing "NComp" components. The thermodynamic
       formulation can be found in the documentation under the ../doc directory.
'''


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

OutFile = open('output', 'w')

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
    ThT.MFrac = 0. ; ThT.PhaseFrac = 0.
    Inc_Phase = 0.25 ; Inc_Comp = 0.1
    NearlyOne = 1. - ThT.Residual

    Niter_Phase = int( 1. / Inc_Phase ) + 1 ; Niter_Comp = int( 1. / Inc_Comp ) + 1
    
    Molar_Gibbs_Free = [ 0. for i in range( Niter_Phase * Niter_Comp ) ]
    Composition  = [ 0. for i in range( Niter_Phase * Niter_Comp ) ] 
    Phase = [ 0. for i in range( Niter_Phase * Niter_Comp ) ]



    """ This is a temporary hack to generate (on-the-fly) molar phase (mole/mass 
             fraction): Vapour (PhaFrac[0]) and Liquid (PhaFrac[1]).             """
    for iter_phase in range( Niter_Phase ):
        ThT.PhaseFrac = NC.Generate_PhaseFraction( iter_phase, Inc_Phase )
        print 'Phase Molar Fraction:', ThT.PhaseFrac


        """ This is a temporary hack to generate (on-the-fly) molar composition
                 of all phases.                                                  """
        for iter_comp in range( Niter_Comp ):
            MolFrac =  NC.Generate_MoleFraction( iter_comp, Inc_Comp )
            print 'Composition:', MolFrac

        print ' '
        print ' '

        


    





