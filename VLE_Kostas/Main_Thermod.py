
#!/usr/bin/env python

import numpy as np
import math
import ThermoTools as ThT


''' This program aims to calculate the vapour-liquid equilibrium composition
       of a mixture containing "Ncomp" components. The thermodynamic formula
       tion can be found in the documentation under the ../doc directory.
'''


# The line below will be uncommented when this code is integrated with the 
#     Simulated Annealing Algorithm code
# def Gibbs( X_Vec, F_Gibbs )




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
# Calculate ...
