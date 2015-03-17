#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math # to explain/use the math functions

# =================== ASSOCIATED/EXTERNAL FUNCTIONS ====================#

def set_Global_Variables(): # Defining gas constant Rconst
    global Rconst 
    Rconst = 8.314 # J/(gmol.K)

def SRK( n, Tc, Pc, T, V, w ): # Defining Soave-Redlich-Kwong EOS
    set_Global_Variables()
    a = 0.42748 * ( Rconst * Tc)**2 / Pc
    b = 0.08664 * ( Rconst * Tc) / Pc
    gamma = 0.48508 + 1.5517 * w - 0.15613 * w**2
    for i in range( n ):
        Tr = T[ i ] / Tc
        alpha = ( 1. + gamma * ( 1. - math.sqrt( Tr )))**2
        P_SRK[ i ] = Rconst * T[i ] / (V - b) - a * alpha/ ((V + b) * V)
    return P_SRK

#def PR( n, Tc, Pc, T, V, w ): # Defining Peng-Robinson EOS


#=============================== MAIN CODE ==================================#

print 'Solving Pressure of gases in equilibrium using a prescribed EOS'
V = 1.e-3 # Initial volume (m^3/mol)
n = 11 # Creating initial dimension for work arrays


T_work = [0 for i in range(n) ] # creating an array of temperature T( 0 : n )
dT = 20.
T_work[ 0 ] = 400. # associating T(0) = 400 K
for i in range ( n - 1 ): # Populating the array T with prescribed temperature values 
    T_work[ i + 1 ] = T_work[ i ] + dT

# Creating numpy array for experimental pressure
P_exp = np.array([ 27., 29.1, 31.3, 33.3, 35.3, 37.4, 39.3, 41.5, 43.3, 45.4, 47.4 ]) # bar
P_exp = P_exp * 1.e5 # Pa

# Now creating the work array
P_SRK = [0. for i in range(n) ]
P_PR = [0. for i in range(n) ]

# Properties for Methane:    
Tc  = 190.6 # K
Pc = 45.99e5 # Pa
w = 0.012 # accentric factor (dimentionless)

# Calling the function to calculate pressure through the SRK EOS
SRK( n, Tc, Pc, T_work, V, w )
print T_work, P_SRK

#PR( n, Tc, Pc, T_work, V, w )
#print T_work, P_PR


# ========== PLOTTING DATA ==========================# 

bplot.title('Experimental Vs. Numerical Solution')        #title of the plot

bplot.plot(T_work, P_SRK, '--', color='g', label='Numerical SRK')           #ploting  ya vs. time in green color
bplot.plot(T_work, P_exp,  '-', color='b', label='Experimental') #ploting yee vs. time in black color
#bplot.plot(T_work, P_PR,  '*', color='r', label='Numerical PR') #ploting yee vs. time in black color


bplot.grid(True)                                      #using this to display the grid
bplot.xlabel('Temperature (K)')                                  #putting labels in x axis
bplot.ylabel('Pressure (Pa)')                                     #putting labels in y axis

bplot.legend(loc = 1)                                 #the location of the legend, 0:upper left corner ang going clockwise -> 1:upper right corner, 2:lower right corner, 3: lower left corner

bplot.show()                                          #command that you put at the end of the plot to make it appear


