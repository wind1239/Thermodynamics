
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math
import ThermoTools as ThT
import GibbsFunction as GibbsF


''' This is the main program that aims to calculate the vapour-liquid equilibrium
       composition of a mixture containing "NComp" components. The thermodynamic
       formulation can be found in the documentation under the ../doc directory.
'''


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

''' Printing the data from the input file '''
print 'Temp of the mixture (initial, final, increment):', ThT.T_System
print 'Pres of the mixture (initial, final, increment):', ThT.P_System
#
print 'Species: ', ThT.Species
print 'Feed Composition: ', ThT.Feed_Composition
print 'Critical Temperature:', ThT.T_Crit
print 'Critical Pressure:', ThT.P_Crit
print 'Molar Mass:', ThT.MolarMass
#
print 'Binary Parameter:', ThT.BinaryParameter


#
# This is a temporary hack for testing the code, as the Simulated
#    Annealing is introduced, MFrac and PhaseFrac will be estimated 
#    during the calculations.
#
# MFrac stores the normalised composition (i.e., mass fractions or mole fractions) as
#    MFrac_{1}(Vap), MFrac_{2}(Vap), ..., MFrac_{NComp}(Vap), MFrac_{1}(Liq), 
#        MFrac_{2}(Liq), ..., MFrac_{NComp}(Liq) 
#
# PhaseFrac contains the mole (or mass) fraction of the phase. Assuming that there 
#    are only 2 phases in equilibrium, Liquid and Vapour, therefore:
#           PhaseFrac[ 0 ] + PhaseFrac[ 1 ] = 1.0
#

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]  
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; MFrac[ 2 ] = 0.20 ; MFrac[ 3 ] = 0.2 # Vapour phase
MFrac[ 4 ] = 0.10; MFrac[ 5 ] = 0.10; MFrac[ 6 ] = 0.20 ; MFrac[ 7 ] = 0.6 # Liquid phase
ThT.Sum2One( 'Mass Fraction (Vapour)', MFrac[ :4 ] )
ThT.Sum2One( 'Mass Fraction (Liquid)', MFrac[ 4: ] )

PhaseFrac =  [ 0. for i in range( ThT.NPhase ) ]
PhaseFrac[ 0 ] = 0.40 ; PhaseFrac[ 1 ] = 0.60 
ThT.Sum2One( 'Phase Fraction', PhaseFrac )



''' Initialising Temperature and Pressure '''
Temp = ThT.T_System[ 0 ]
Press = ThT.P_System[ 0 ] 

'''
     MICHAELSEN'S STABILITY TEST: '''
#Alpha_V = math.fsum( ThT.Z_Feed - 

Gibbs_Free = GibbsF.Calc_Gibbs( Temp, Press, MFrac, PhaseFrac )

print 'Gibbs Free Energy:', Gibbs_Free


""" This Loop can be used later to obtain equilibrium composition in a 
      a wide range of temperature and pressure

# Loop for temperature:
while Temp <= ThT.T_System[ 1 ]:

    # Loop for pressure:
    Press = ThT.P_System[ 0 ] # Initialising Pressure
    while Press <= ThT.P_System[ 1 ]:

       # Gibbs_Free = GibbsF.Gibbs_Function( Temp, Press, MFrac, PhaseFrac )
        a = 1.
        print Temp, Press



 ###
        Press = Press + ThT.P_System[ 2 ] # End of pressure loop


###
    Temp = Temp + ThT.T_System[ 2 ]+1.e-6 # End of temperature loop

print 'Final Press:', Press

"""

