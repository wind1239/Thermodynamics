
#!/usr/bin/env python

import numpy as np
import math
import ThermoTools as ThT


''' This is the main program that aims to calculate the vapour-liquid equilibrium
       composition of a mixture containing "Ncomp" components. The thermodynamic
       formulation can be found in the documentation under the ../doc directory.
'''


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

''' Printing the data from the input file '''
print 'Species: ', ThT.Species
print 'Temp of the mixture (initial, final, increment):', ThT.T_System
print 'Pres of the mixture (initial, final, increment):', ThT.P_System
print 'Temperature:', ThT.T_Crit
print 'Pressure:', ThT.P_Crit
print 'Molar Mass:', ThT.MolarMass
print 'Binary Parameter:', ThT.BinaryParameter


''' Initialising Temp an Press: '''
Temp = ThT.T_System[ 0 ]
Press = ThT.P_System[ 0 ]

# This is a temporary hack for testing the code, as the Simulated
#    Annealing is introduced, MassFrac and LiqFrac will be estimated 
#    dduring the calculations
MassFrac = [ 0. for i in range( Ncomp * 2 ) ]
LiqFrac =  [ 0. for i in range( Ncomp * 2 ) ]
for i in range( Ncomp - 1 ):
    MassFrac[ i ] = 0.30
    LiqFrac[ i ] = 0.25




# Loop for temperature:
while Temp <= ThT.T_System[ 1 ]:

    # Loop for pressure:
    While Press <= ThT.P_System[ 1 ]:

        Gibbs_Free = Gibbs_Function( Temp, Press, MassFrac, LiqFrac )



 ###
        Press = Press + ThT.P_System[ 2 ] # End of pressure loop


###
    Temp = Temp + ThT.T_System[ 2 ] # End of temperature loop
