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

print
print
print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
print
print

                                                  
ThT.ReadSet_Global_Variables()                             # reading the external file 

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]    # create an a array with 0 values for the MFrac    

c1 = np.linspace(10e-5, 0.9999, 5)   # + - 25%
#c1 = np.linspace(0.7478, 0.9999, 10)
#c1 = np.linspace(0.7487, 0.9999, 10)
#c1 = np.linspace(0.7499, 0.9999, 10)

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

for i in range( nk ):
    for j in range( nk ):
        node = i * nk + j
        c1b[ node ] = c1[ i ] #; print c1b[ node ]  
        c2b[ node ] = c2[ j ] #; print c2b[ node ]

Gibbsk = [ 0. for k in range( nk ) ]
sumfeedk = [ 0. for k in range( nk ) ]
MolarGibbsk = [ 0. for k in range( nk ) ]
finalGibbsk = [ 0. for k in range( nk ) ]
MFrac = [ 0.01 for i in range( ThT.NComp * ThT.NPhase ) ]
chempot = [0. for i in range(ThT.NComp**2) ]


for i in range(nk):
    for j in range(nk):
        node = i * nk + j
        for iphase in range(ThT.NPhase):
            if iphase == 0:
               print
               print '  you are at the Vapor phase '        

        MFrac[0] = c1b[node];     print ' the MFrac[0] = ', MFrac[0]
        MFrac[1] = 1 - c1b[node]; print ' the MFrac[1] = ', MFrac[1]
        MFrac[2] = c2b[node];     print ' the MFrac[2] = ', MFrac[2]
        MFrac[3] = 1 - c2b[node]; print ' the MFrac[3] = ', MFrac[3]
        print ' ------------------------------- '
               
        node_init = iphase * ThT.NComp ; node_final = iphase * ThT.NComp + ThT.NComp; 
        #print ' you are at the node ', node_init , node_final            
        ChemPot = [0. for z in range(ThT.NComp * ThT.NPhase) ]  # Set up an array of chemical potential for each component at each phase (dimension NComp * NPhase )
        #print ' ChemPot = ', ChemPot
        if i == j: 
           #print '  for the component ', ThT.Species[i]
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
               Gibbsk[node] = sumGibbs + ( PhaseFrac[ Lphase ] * MFrac[ nodeL ]  * ( ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) - ( ChemPot[ nodeLfinal ]  - ChemPot[ nodeVfinal ] ) ) #+\
              #PhaseFrac[ Lphase ] * ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) )
               sumfeedk[node] = sumfeed + ThT.Z_Feed[icomp] * ChemPot[ nodeV ]
              #print ' the sumGibbs = ', Gibbsk[k] , ' and the sumfeed = ', sumfeed 

            MolarGibbsk[node] = Gibbsk[node] + sumfeedk[node]
        finalGibbsk[node] = - MolarGibbsk[node]


print ' =>', finalGibbsk[node]
       





#print ' the Molar Gibbs = ', finalGibbsk 

c1, c2 = np.meshgrid( c1, c2 )
print ' the ci - c2 matrix : ' , c1, c2 

#print c1[1][1], '####',c1.shape
#print c2[1][1], '####',c1.shape 


finalGibbsMatrix = np.zeros( (nk, nk) )
for i in range( nk ):
    for j in range( nk):
        node = i * nk + j
        finalGibbsMatrix[i][j] = finalGibbsk[node]

print ' finalGibbsMatrix[i][j] ', finalGibbsMatrix[i][j]



m = [ 0. for k in range( nk ) ]
m = np.argsort(finalGibbsk)
#print ' m = ', m 

s1min2 = c1[m[0]] ; s2min2 = c2[m[0]] ; gmin2 = finalGibbsk[m[0]]
print ' '; print ' c1 and c2:', s1min2, s2min2, 'for min Gibbs of:', gmin2





'''
exp_value_c1 = raw_input(' exp_value_c1 ? ')
exp_value_c1 = float(exp_value_c1)

exp_value_c2 = raw_input(' exp_value_c2 ? ')
exp_value_c2 = float(exp_value_c2)
 
print ' error 1 = ', ( exp_value_c1 - c1[m[0]] / exp_value_c1 ) * 100 , ' % '
print ' error 2 = ', ( exp_value_c2 - c2[m[0]] / exp_value_c2 ) * 100 , ' % '
'''



###
### Script for Plotting
###

fig = plt.figure()
ax = fig.gca(projection='3d')
ContourPlot = False
if ContourPlot:
    ax.plot_surface( c1, c2, gmix, rstride=8, cstride=8, alpha=0.3 )
    cset = ax.contourf( c1, c2, gmix, zdir='z', offset=-10000, cmap=cm.coolwarm)
    cset = ax.contourf( c1, c2, gmix, zdir='x', offset=-40, cmap=cm.coolwarm)
    cset = ax.contourf( c1, c2, gmix, zdir='y', offset=40, cmap=cm.coolwarm)
    ax.set_xlabel('X')
    ax.set_xlim( 0, 1)
    ax.set_ylabel('Y')
    ax.set_ylim( 0, 1)
    ax.set_zlabel('Z')
    ax.set_zlim(-1000, 1000)
else:
    surf = ax.plot_surface( c1, c2, gmix, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False )
    #ax.plot_surface( c1, c2, gmix, rstride=1, cstride=1, cmap=cm.coolwarm,linewidth=0, antialiased=False )
    ax.set_zlim(-1000, 1000)
    ax.zaxis.set_major_locator(LinearLocator(10))
    ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
    fig.colorbar(surf, shrink=0.5, aspect=5)

plt.show()


      
            


          
            
         
           
  

          
