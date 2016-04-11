#!/usr/bin/env python

import numpy as np
import math
import sys
import thermotools_test as ThT
import calculate_EOS_PR_test as PR
import calculate_ln_gamma_test as lng
import calculate_fi_test as fi
import calculate_terms_test as terms
import calculate_chemical_potential_test as chemp
import pylab 
import time

print
print
print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
print
print

                                                  
ThT.ReadSet_Global_Variables()  # reading the external file 

Rconst = 8.314                  # Gas constant [J/(gmol.K)]

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]          # setting up the size of the array/vector of the molar fractions
print '  the initial molar fraction before reading from the input.dat is', MFrac


MFrac[ 0 ] = 0.20; MFrac[ 1 ] = 0.8  ; # Vapour phase             # 2 components in 2 phases means an array of size 4
                                                                  # concentration range from mole fractions of 0.2 - 1 of methane 
#MFrac[ 2 ] = 0.30; MFrac[ 3 ] = 0.10; # Liquid phase             # the first 2 for the Vapour, the second 2 for the Liquid 
print '  the MFrac = ', MFrac


#aij = [0. for i in range(ThT.NComp**2) ]
chempot = [0. for i in range(ThT.NComp**2) ]                    # the size of the array to save the chem.potential values has the size of 4  


# = = = Claculate the condition that sais that Summations of MFrac = 1.0 = = = 
MFrac_sum = 0
for i in range(ThT.NComp*ThT.NPhase):
    MFrac_sum = MFrac_sum + MFrac[ i ]
print '  the MFrac_sum = ', MFrac_sum 

if MFrac_sum == 1:
   print'  we meet the condition so below we list the species'
   pass
else:
   exit

print
print '  --------------------------------------------------------------------------------------------------------------'
print '     The number of Species is: ', ThT.NComp , ' while the Species are : ', ThT.Species , ' and the number of phases are :', ThT.NPhase 
print '  --------------------------------------------------------------------------------------------------------------'
print


for iphase in range(ThT.NPhase):
    print '                                                                                               ############################### '
    print '                                                                                                   you are at the iphase ', iphase 
    print '                                                                                               ############################### '
    for i in range(ThT.NComp):
        for j in range(ThT.NComp):
            node = i * ThT.NComp + j
            print '  -------------------------------------------------------------------------------------------------------------------- '
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

            node_init = iphase * ThT.NComp ; node_final = iphase * ThT.NComp + ThT.NComp; 
            #print node_init , node_final            
            ChemPot = [0. for z in range(ThT.NComp * ThT.NPhase) ]  # Set up an array of chemical potential for each component at each phase (dimension NComp * NPhase )       
            
            if i == j: 
               print '  for the component ', ThT.Species[i]
               #for iphase in range( ThT.NPhase ):
               print MFrac[ node_init:node_final ], node_init, node_final
               ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )   # This function will return the chemical potential of phase IPhase 
               print ' the ChemPot = ', ChemPot[ node_init:node_final ]                                          # components and then it can be be operated to obtain the molar Gibbs energy.
               print
            else:                                                       # this is for the rest of the elements of the square matrix of the aij
               print '  i am in the else case ' #' for the component ', ThT.Species[i],' with respect to' , ThT.Species[j]
               print
            print 
            time.sleep(0)



""" Calculating Gibbs molar """
PhaseFrac = [0. for i in range(ThT.NPhase) ] 

PhaseFrac[ 0 ] = 0.35 ;  PhaseFrac[ 1 ] = 1. - PhaseFrac[ 0 ]

sumGibbs = 0. ;  sumfeed = 0.

for icomp in range( ThT.NComp ):
    Vphase = 0 ; Lphase = 1 
    nodeV = Vphase * ThT.NComp + icomp ; nodeL = Lphase * ThT.NComp + icomp             # a clever way to describe the nodes for Vapour and liquid phases
    nodeVfinal = Vphase * ThT.NComp + ThT.NComp - 1; nodeLfinal = Lphase * ThT.NComp + ThT.NComp - 1; 
    if icomp <= ThT.NComp - 2:
       sumGibbs = sumGibbs + ( PhaseFrac[ Lphase ] * MFrac[ nodeL ]  * ( ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) - ( ChemPot[ nodeLfinal ]  - ChemPot[ nodeVfinal ] ) ) + \
                     PhaseFrac[ Lphase ] * ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) )
    sumfeed = sumfeed + ThT.Z_Feed[icomp] * ChemPot[ nodeV ]
    print ' the sumGibbs = ', sumGibbs , ' and the sumfeed = ', sumfeed 

MolarGibbs = sumGibbs + sumfeed
print ' the Molar Gibbs = ', MolarGibbs    

      
            


          
            
         
           
  

          
