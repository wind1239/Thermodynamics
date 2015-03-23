
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions
import ThermoTools as ThT
import MixingRule_Simple as MixS
import EOS_PR as PR


#=============================== MAIN CODE ==================================#

print 'Solving Peng-Robinson for a mixture of fluids using simple mixing rules'
ThT.set_Global_Variables()

print 'Fluid \t\t  MW (g/gmol) \t  Tc (K) \t  Pc (Pa) \t  w \t\t    x  '
for index in range( ThT.Nc ):
    print ThT.species[ index ],'\t\t', ThT.MW[ index ], '\t\t',  ThT.Tc[ index ], '\t\t', ThT.Pc[ index ], '\t', ThT.w[ index ], '\t\t', ThT.FeedMoleFraction[ index ]
print ' '

print 'Binary interaction parameter (Kij):', ThT.kij[1]
print ' '

Composition = [ 0. for i in range( ThT.Nc ) ]

# Creating Temperature and Pressure arrays:
nt = 5
T_work = np.array( [ 298.15, 350., 400., 450., 500. ] )

np = 3 ; Pmax = 450. ; Pmin = 1.
P_work = [ 0. for i in range( np ) ] # creating an array of pressure
dP = ( Pmax - Pmin ) * 1.e5 / float( np - 1 )
#dP = 1.5
P_work[ 0 ] = 1.e5 # associating P(0) = 10^5 Pa = 1 bar
for i in range ( np - 1 ): # Populating the array T with prescribed temperature values 
    P_work[ i + 1 ] = P_work[ i ] + dP


P_bar = [0. for i in range( np ) ]
for i in range( np ):
    P_bar[ i ] = P_work[ i ] * 1.e-5

Zvapour = [ 0. for i in range( np * nt ) ]
Zliquid = [ 0. for i in range( np * nt ) ]

for j in range( nt ):
    MixingParameters = [0. for i in range( 2 ) ]
    MixingParameters = MixS.MixingRules( T_work[ j ], ThT.FeedMoleFraction ) # Calculating am and bm through simple mixing rules based on individual attractive and repulsive binary parameters
    for i in range( np ):
        node = np * j + i 
        ( Zvapour[ node ], Zliquid[ node ] ) = PR.Cubic_PR( T_work[ j ], P_work[ i ], MixingParameters[ 0 ], MixingParameters[ 1 ] )
    print ' '



# Now, add here functions to calculate molar volume of the gaseous mixture and specific volume of chemical species at reservoir conditions

# ========== PLOTTING DATA ==========================# 

bplot.title('Pressure X Compressibility')        #title of the plot

for j in range( nt ):
    node1 = np * j + 0 ; node2 = np * j + np - 1
    if j == 0:
        colour = 'k-'  
    elif j == 1:
        colour = 'b-o' 
    elif j == 2:
        colour = 'r-'  
    elif j == 3:
        colour = 'r--' 
    elif j == 4:
        colour = 'g-s'  

    text = 'T = ' + str( T_work[ j ] ) + ' K'

    bplot.plot(P_bar, Zvapour[ node1 : node2 + 1 ], colour, label= text)           #ploting  ya vs. time in green color


bplot.grid(True)                                      #using this to display the grid
bplot.xlabel('Pressure (bar)')                                  #putting labels in x axis
bplot.ylabel('Z')                                     #putting labels in y axis

bplot.legend(loc = 0)                                 #the location of the legend, 0:upper left corner ang going clockwise -> 1:upper right corner, 2:lower right corner, 3: lower left corner

bplot.show()                                          #command that you put at the end of the plot to make it appear

