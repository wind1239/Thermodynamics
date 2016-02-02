# my test to understand what I want to call and where ...

import matplotlib.pyplot as bplot
import numpy as np
import math
import ThermoTools as ThT
import GibbsFunction as GibbsF
#need to work on the GibbsF


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

# Printing the data from the input file:
print '= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = ='
print 'Species: ', ThT.Species
print 'Temp of the mixture (initial, final, increment):', ThT.T_System
print 'Pres of the mixture (initial, final, increment):', ThT.P_System
print 'Temperature:', ThT.T_Crit
print 'Pressure:', ThT.P_Crit
print 'Molar Mass:', ThT.MolarMass
print 'Binary Parameter:', ThT.BinaryParameter
print '= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = ='

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
print 'the molar fraction is', MFrac
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; MFrac[ 2 ] = 0.20 ; MFrac[ 3 ] = 0.2 # Vapour phase
MFrac[ 4 ] = 0.10; MFrac[ 5 ] = 0.10; MFrac[ 6 ] = 0.20 ; MFrac[ 7 ] = 0.6 # Liquid phase
ThT.Sum2One( 'Mass Fraction (Vapour)', MFrac[ :4 ] )
ThT.Sum2One( 'Mass Fraction (Liquid)', MFrac[ 4: ] )

print
print ' - - - - - up to here we are good - - - - - '
print
