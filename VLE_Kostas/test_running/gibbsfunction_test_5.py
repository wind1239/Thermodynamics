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
import pylab as pl 
import time 

print
print
print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
print
print

                                                  
ThT.ReadSet_Global_Variables()                             # reading the external file 

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]    # create an a array with 0 values for the MFrac    

c1 = np.linspace(0.25,0.35,100)
c2 = []
for x in c1:
    c2.append(1.0-x)

c2 = np.array(c2)

print ' component 1 = ', c1
print
print ' component 2 = ', c2
print

nk = len(c1)

Gibbsk = [ 0. for k in range( nk ) ]
sumfeedk = [ 0. for k in range( nk ) ]
MolarGibbsk = [ 0. for k in range( nk ) ]
finalGibbsk = [ 0. for k in range( nk ) ]
for k in range( nk ):
    MFrac = [ 0.01 for i in range( ThT.NComp * ThT.NPhase ) ]
    
    MFrac[0] = c1[k];     #print ' the MFrac[0] = ', MFrac[0]
    MFrac[1] = 1 - c1[k]; #print ' the MFrac[1] = ', MFrac[1]
    MFrac[2] = c2[k];     #print ' the MFrac[2] = ', MFrac[2]
    MFrac[3] = 1 - c2[k]; #print ' the MFrac[3] = ', MFrac[3]
    #print ' ------------------------------- '
    
    for iphase in range(ThT.NPhase):
        if iphase == 0:
           print
           print '  you are at the Vapor phase '
        
           chempot = [0. for i in range(ThT.NComp**2) ]
           for i in range(ThT.NComp):
               for j in range(ThT.NComp):
                   node = i * ThT.NComp + j
                   node_init = iphase * ThT.NComp ; node_final = iphase * ThT.NComp + ThT.NComp; 
                   #print ' you are at the node ', node_init , node_final            
                   ChemPot = [0. for z in range(ThT.NComp * ThT.NPhase) ]  # Set up an array of chemical potential for each component at each phase (dimension NComp * NPhase )
                   #print ' ChemPot = ', ChemPot
                   if i == j: 
                      print '  for the component ', ThT.Species[i]
                      print MFrac[ node_init:node_final ], node_init, node_final
                      ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )   
                   else:                                                       
                      print '  i am in the else case ' #' for the component ', ThT.Species[i],' with respect to' , ThT.Species[j]
                      ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )
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
           Gibbsk[k] = sumGibbs + ( PhaseFrac[ Lphase ] * MFrac[ nodeL ]  * ( ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) - ( ChemPot[ nodeLfinal ]  - ChemPot[ nodeVfinal ] ) ) + \
                     PhaseFrac[ Lphase ] * ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) )
           sumfeedk[k] = sumfeed + ThT.Z_Feed[icomp] * ChemPot[ nodeV ]
        #print ' the sumGibbs = ', Gibbsk[k] , ' and the sumfeed = ', sumfeed 

        MolarGibbsk[k] = Gibbsk[k] + sumfeedk[k]
        finalGibbsk[k] = - MolarGibbsk[k]
        

print ' the Molar Gibbs = ', finalGibbsk 
print ' Gibbs sorted = ', np.sort(finalGibbsk)

gmin = min(finalGibbsk)
print ' Gmin = ', gmin
print
c1min = min(c1) 
print ' c1 min = ', c1min 
print ' c1 sorted = ', np.sort(c1)
print 
c2min = min(c2)
print ' c2 min = ', c2min
print ' c2 sorted = ', np.sort(c2)
print


#####################################################################
pl.title(' Gibbs vs. molar fraction of componenets A and B ')

# make axis labels
pl.xlabel('x axis - molar fraction/components')
pl.ylabel('y axis - Gibbs')

component1 = pl.plot(c1,finalGibbsk, '-' )
component2 = pl.plot(c2,finalGibbsk, '--')
pl.plot(c1min, gmin, 'o' )
# set axis limits
#pl.xlim(0.0, 1.0)
#pl.ylim(0.0, 30.)
pl.legend( [component1, component2], ('component1', 'component2') )
pl.grid()
pl.show() 




      
            


          
            
         
           
  

          
