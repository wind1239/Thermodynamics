
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions
import ThermoTools
import MixingRule_Simple
import EOS_PR


#=============================== MAIN CODE ==================================#

print 'Solving Peng-Robinson for a mixture of fluids using simple mixing rules'
set_Global_Variables()

print ' '
print 'Fluid \t\t  MW (g/gmol) \t  Tc (K) \t  Pc (Pa) \t  w \t\t    x  '
for index in range( Nc ):
    print species[ index ],'\t\t', MW[ index ], '\t\t',  Tc[ index ], '\t\t', Pc[ index ], '\t', w[ index ], '\t\t', FeedMoleFraction[ index ]
print ' '

print 'Binary interaction parameter (Kij):', kij[1]
print ' '

MixingParameters = [0 for i in range(2) ]
T = 300. ; P = 1.e7
print 'T:', T,'K and P:', P/1.e5, 'bar'

MixingParameters = MixingRules(T) # Calculating am and bm through simple mixing rules based on individual attractive and repulsive binary parameters

Zvapour = Cubic_PR( T, P, MixingParameters[0], MixingParameters[1] )
print ' '

print 'Zvapour: ', Zvapour

# Now, add here functions to calculate molar volume of the gaseous mixture and specific volume of chemical species at reservoir conditions
