
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math
import ThermoTools as ThT
import GibbsFunction as GibbsF
import PhaseStability as Michaelsen


''' This is the main program that aims to calculate the vapour-liquid equilibrium
       composition of a mixture containing "NComp" components. The thermodynamic
       formulation can be found in the documentation under the ../doc directory.
'''


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

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


'''
===============================================================
     MICHAELSEN'S STABILITY TEST: Decision of the Phases 
===============================================================
                                                            '''
GZero = Michaelsen.Phase_Stability( Temp, Press )


print 'GZero:', GZero





#Gibbs_Free = GibbsF.Calc_Gibbs( Temp, Press )

#print 'Gibbs Free Energy:', Gibbs_Free


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

