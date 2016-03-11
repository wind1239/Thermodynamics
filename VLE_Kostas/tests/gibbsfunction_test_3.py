#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import pylab 
import time


Rconst = 8.314 # Gas constant [J/(gmol.K)]


print
print
print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
print
print


# = = = Reading Input data from external file = = =                                                  
ThT.ReadSet_Global_Variables() 

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
print '  the initial molar fraction before reading from the input.dat is', MFrac

# = = = A vector with MFrac values - molar fraction = = =
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
MFrac[ 2 ] = 0.30; MFrac[ 3 ] = 0.10; # Liquid phase
print '  the MFrac = ', MFrac


# = = = Claculate the condition that sais that Summations of MFrac = 1.0 = = = 
MFrac_sum = 0
for i in range(ThT.NComp*ThT.NPhase):
    MFrac_sum = MFrac_sum + MFrac[ i ]
print '  the MFrac_sum = ', MFrac_sum 

if MFrac_sum == 1:
   pass
   print'  we meet the condition so below we list the species'
else:
   exit

print
print '  --------------------------------------------------------------------------------------------------------------'
print '     The number of Species is: ', ThT.NComp , ' while the Species are : ', ThT.Species , ' and the number of phases are :', ThT.NPhase 
print '  --------------------------------------------------------------------------------------------------------------'
print

for iphase in range(ThT.NPhase):
    print
    print '                                                                                               you are at the iphase ', iphase 
    print
    for i in range(ThT.NComp):
        for j in range(ThT.NComp):
            node = i * ThT.NComp + j
            print '  ===================================================================================================================== '
    	    print '  you are at the component ' , i, j, ' at the node = ', node, 'at the ', iphase,' phase'
            print 
            print '  for the  ', ThT.Species[i] 
    	    print '  T_Crit = ', ThT.T_Crit[i]
    	    print '  P_Crit = ', ThT.P_Crit[i]            
    	    print '  wmega  = ', ThT.Accentric_Factor[i]       # accentric factor 
    	    print '  Kij    = ', ThT.BinaryParameter[node]     # the binary parameter 
    	    print '  Lij    = ', ThT.Lamda_wilson[node]        # the lamda parameter for the ln_gamma term
            print '  xi     = ', ThT.MolarMass[i]              # the molar mass 
    	    print '  zi     = ', ThT.Z_Feed[i]                 # overall feed mass fraction of the component 
    	    print ''
            
            # = = = calculate the chemical potential for each phase = = =
           
  

          
