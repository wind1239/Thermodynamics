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
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter
import matplotlib.pyplot as plt
import time 

###
### Functions Call_Calc_ChemPot
###
def Call_Calc_ChemPot( MFrac ):      # This function will calculate the ChemPotential, the input is MolFraction
    for iphase in range(ThT.NPhase): # Loop for Phases
        node_init = iphase * ThT.NComp ; node_final = iphase * ThT.NComp + ThT.NComp 
                                     # For phase 0, 1st component : node_init = 0 & node_final = 0 * 1 + 1 = 1
                                     # For phase 1, 2nd component : node_init = 2 & node_final = 1 * 2 + 2 = 4 
        ChemPot = [0. for z in range(ThT.NComp * ThT.NPhase) ] # Creating a temporary array ChemPot

        print MFrac[ node_init:node_final ], node_init, node_final
        ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )
        # ChemPot[ 0 : 1 ] = chemp.Calc_ChemPot( iphase, MFrac[ 0 : 1 ] ) 
    # End Loop for Phases
    return ChemPot

###
### Function Call_Calc_Gibbs:
###
def Call_Calc_Gibbs( MFrac, ChemPot ): # This function will calculate the Gibbs energy, the input is the MolFraction and the ChemPotential from the function above
    
    # Setting molar fraction of the phases in equilibrium 
    PhaseFrac = [0. for k in range( ThT.NPhase ) ] 

    # This needs to be generalised later on ... for now it is just a quick hack 
    Temp = MFrac[ 0 ] - MFrac[ 2 ]
    if ( abs( Temp - 0. ) <= 1.e-10 ):
        sys.exit()
    else:
        PhaseFrac[ 0 ] = 0.9 ; PhaseFrac[ 1 ] = 1. - PhaseFrac[ 0 ]
        # PhaseFrac[ 0 ] = Vapour Phase = 0.9 ; PhaseFrac[ 1 ] = Liquid Phase = 1 - Vapour Phase = 0.1  
        # the line above describes the condition for the feed composition zi = V * yi + (1 - V) * xi
    
    sumGibbs = 0. ; sumfeedk = 0.
    for icomp in range( ThT.NComp ): # Loop ICOMP
        Vphase = 0 ; Lphase = 1 
        nodeV = Vphase * ThT.NComp + icomp ; nodeL = Lphase * ThT.NComp + icomp 
        # A clever way to describe the nodes for Vapour and liquid phases
        nodeVfinal = Vphase * ThT.NComp + ThT.NComp - 1; nodeLfinal = Lphase * ThT.NComp + ThT.NComp - 1
        
        if icomp <= ThT.NComp - 2: # Conditional ICOMP
            sumGibbs = sumGibbs + PhaseFrac[ Lphase ] * MFrac[ nodeL ]  * ( ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) - ( ChemPot[ nodeLfinal ]  - ChemPot[ nodeVfinal ] ) )
        # End of Conditional ICOMP
        
        sumfeedk = sumfeedk + ThT.Z_Feed[icomp] * ChemPot[ nodeV ]
    # End of Loop ICOMP
    Gibbs = sumGibbs + sumfeedk
    return Gibbs



print
print ' = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = '
print

                                                  
ThT.ReadSet_Global_Variables()                             # reading the external file 

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]    # create an a array with 0 values for the MFrac    

#...
#...
#...
''' comment or uncomment these lines if you want to print or not the componenet 1 & 2 as arrays

c1 = np.linspace(0.1, 0.9999, 25)   # + - 25%
c2 = []
#c2 = np.linspace(0.5891, 0.9818, 5 ) 
for x in c1:
    c2.append(1.0-x)

c2 = np.array(c2)

print ' component 1 = ', c1
print
print ' component 2 = ', c2
print

nk = len(c1)

c1b = [ 0. for k in range( nk*nk ) ] #; print ' c1b = ', c1b
c2b = [ 0. for k in range( nk*nk ) ] #; print ' c1b = ', c1b
gmix = [ 0. for k in range( nk*nk ) ]

    comment or uncomment these lines if you want to print or not the componenet 1 & 2  as arrays'''
#...
#...
#...


MFrac = [ 0.01 for i in range( ThT.NComp * ThT.NPhase ) ]
MFrac[0] = 0.2944 ; MFrac[1] = 1 - MFrac[0]
MFrac[2] = 0.1 ; MFrac[3] = 1 - MFrac[2]

''' Calculating ChemPot for both phases '''
ChemPot = Call_Calc_ChemPot( MFrac ) # ChemPot has dimension (NPhase * NComp) 

''' Now calculating the molar Gibbs energy for the tuple (MFrac[0], MFrac[2]) = (c1b[node],c2b[node])  '''  
MolarGibbs =  -Call_Calc_Gibbs( MFrac, ChemPot ) 

print '  - - - - - - - - - - - - - - - '
print '  MolarGibbs = ', MolarGibbs / 100
print '  - - - - - - - - - - - - - - - '

if False: 
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    ContourPlot = True
    if ContourPlot:
        ax.plot_surface( c1, c2, finalGibbsMatrix, rstride=8, cstride=8, alpha=0.3 )
        cset = ax.contourf( c1, c2, finalGibbsMatrix, zdir='z', offset=-10000, cmap=cm.coolwarm)
        cset = ax.contourf( c1, c2, finalGibbsMatrix, zdir='x', offset=-40, cmap=cm.coolwarm)
        cset = ax.contourf( c1, c2, finalGibbsMatrix, zdir='y', offset=40, cmap=cm.coolwarm)
        ax.set_xlabel(' component 1 ')
        ax.set_xlim( 0, 1)
        ax.set_ylabel(' component 2 ')
        ax.set_ylim( 0, 1)
        ax.set_zlabel(' Gibbs ')
        ax.set_zlim(-10000, 10000)
    else:
        surf = ax.plot_surface( c1, c2, finalGibbsMatrix, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False )
        #ax.plot_surface( c1, c2, finalGibbsMatrix, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False )
        ax.set_zlim(-1000, 1000)
        ax.zaxis.set_major_locator(LinearLocator(10))
        ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
        fig.colorbar(surf, shrink=0.5, aspect=5)

    plt.show()




            
         
           
  

          
