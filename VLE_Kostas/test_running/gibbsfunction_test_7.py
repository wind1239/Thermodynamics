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
from mpl_toolkits.mplot3d import axes3d
import matplotlib.pyplot as plt
from matplotlib import cm
import time 

print
print
print ' = = = = = = = = = = = = = = = = = = = = = = = = = BEGIN OF THE GIBBS CALCULATIONS = = = = = = = = = = = = = = = = = = = = = = = = '
print
print

                                                  
ThT.ReadSet_Global_Variables()                             # reading the external file 

MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]    # create an a array with 0 values for the MFrac    

c1 = np.linspace(0.7478, 0.9999, 10)   # + - 25%
#c1 = np.linspace(0.7478, 0.9999, 10)
#c1 = np.linspace(0.7487, 0.9999, 10)
#c1 = np.linspace(0.7499, 0.9999, 10)

c2 = []
#c2 = np.linspace(10e-5, 0.9999, 10) 
for x in c1:
    c2.append(1.0-x)

c2 = np.array(c2)

#print ' component 1 = ', c1
#print
#print ' component 2 = ', c2
#print

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
gmix = [ 0. for k in range( nk*nk ) ]

#for k in range( nk ):
MFrac = [ 0.01 for i in range( ThT.NComp * ThT.NPhase ) ]

for iphase in range(ThT.NPhase):
    if iphase == 0:
       print
       print ' you are at the Vapor phase '
    else:  
       print
       print ' you are at the Liquid phase ' 
        
    chempot = [0. for i in range(ThT.NComp**2) ]
    #print chempot
    
    for i in range(nk):
        for j in range(nk):
            node = i * nk + j
            for k in range(ThT.NComp):
                MFrac[0] = c1b[node];     #print ' the MFrac[0] = ', MFrac[0]
                MFrac[1] = 1 - c1b[node]; #print ' the MFrac[1] = ', MFrac[1]
                MFrac[2] = c2b[node];     #print ' the MFrac[2] = ', MFrac[2]
                MFrac[3] = 1 - c2b[node]; #print ' the MFrac[3] = ', MFrac[3]
                #print ' ------------------------------- '

                node_init = iphase * ThT.NComp ; node_final = iphase * ThT.NComp + ThT.NComp;
                print 
                print ' you are at the node ', node, node_init , node_final            
                ChemPot = [0. for z in range(ThT.NComp * ThT.NPhase) ]  # Set up an array of chemical potential for each component at each phase (dimension NComp * NPhase )
                print ' ChemPot = ', ChemPot
                if i == j: 
                   print '  for the component ', i
                   #print MFrac[ node_init:node_final ], node_init, node_final
                   ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )   
                else:                                                       
                   print '  i am in the else case for the component ', i ,' with respect to' , j
                   ChemPot[ node_init:node_final ] = chemp.Calc_ChemPot( iphase, MFrac[ node_init : node_final ] )
                   print 
                   #time.sleep(0)   

    """ Calculating Gibbs molar """
    PhaseFrac = [0. for i in range(ThT.NPhase) ] 
    PhaseFrac[ 0 ] = 0.35 ;  PhaseFrac[ 1 ] = 1. - PhaseFrac[ 0 ]
    sumGibbs = 0. ;  sumfeed = 0.

    for icomp in range( ThT.NComp ):
        Vphase = 0 ; Lphase = 1 
        nodeV = Vphase * ThT.NComp + icomp ; nodeL = Lphase * ThT.NComp + icomp             # a clever way to describe the nodes for Vapour and liquid phases
        nodeVfinal = Vphase * ThT.NComp + ThT.NComp - 1; nodeLfinal = Lphase * ThT.NComp + ThT.NComp - 1; 
        if icomp <= nk - 2:
            Gibbsk[k] = sumGibbs + ( PhaseFrac[ Lphase ] * MFrac[ nodeL ]  * ( ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) - ( ChemPot[ nodeLfinal ]  - ChemPot[ nodeVfinal ] ) ) + \
                     PhaseFrac[ Lphase ] * ( ChemPot[ nodeL ]  - ChemPot[ nodeV ] ) )
            sumfeedk[k] = sumfeed + ThT.Z_Feed[icomp] * ChemPot[ nodeV ]
            #print ' the sumGibbs = ', Gibbsk[k] , ' and the sumfeed = ', sumfeed 

            MolarGibbsk[k] = Gibbsk[k] + sumfeedk[k]
            finalGibbsk[k] = - MolarGibbsk[k]
            
    gmix[node] = -MolarGibbsk[k] ; print ' the gmix = ', gmix[ node ]       

#print ' the Molar Gibbs = ', finalGibbsk 



'''
gmin = min(finalGibbsk)
print ' Gmin = ', gmin
print ' Gmix = ', gmix
c1min = min(c1) 
print ' c1 min = ', c1min, 
print ' c1 sorted = ', np.sort(c1), np.argsort(c1), c1[np.argsort(0)]
print 
c2min = min(c2)
print ' c2 min = ', c2min
print ' c2 sorted = ', np.sort(c2), np.argsort(c2), c2[np.argsort(0)] 
print

#print ' the Molar Gibbs = ', finalGibbsk 
print ' Gibbs sorted = ', np.sort(finalGibbsk)
print ' Gibbs argsort : ' , np.argsort(finalGibbsk), len(np.argsort(finalGibbsk))
print
'''

m = [ 0. for k in range( nk ) ]
m = np.argsort(finalGibbsk)
print ' m = ', m 

'''
s1min = [ 0. for k in range( nk ) ]
s2min = [ 0. for k in range( nk ) ]
GMIN  = [ 0. for k in range( nk ) ]
for i in range( nk ):
    #GMIN =  np.argsort( finalGibbsk[ m[i] ] )
    s1min = c1[ m[ i ] ]; print ' c1 ', c1[ m[ i ] ]
    s2min = c2[ m[ i ] ]; print ' c2 ', c2[ m[ i ] ]; print ' ', m[ i ]; print ' ',  m; print ' ' # np.argmax(GMIN[ i ]) 
print ' for the m = ', m,' the c1 = ', s1min, ' the c2 = ', s2min, ' for the Gibss min = ', GMIN
print 
'''

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



'''
#list=[1.1412, 4.3453, 5.8709, 0.1314]
#print ' the list = ', list.index(min(list)) # Will give you first index of minimum.

#print ' - - - - - - - - - - - - - - - - - - - - - - '
#my_indexed_list = zip(list, range(len(list)))
#print ' my_indexed_list = ', my_indexed_list

#min_value, min_index = min(my_indexed_list); print ' min_value, min_index = ', min_value, min_index 
#max_value, max_index = max(my_indexed_list); print ' max_value, max_index = ', max_value, max_index
#print ' - - - - - - - - - - - - - - - - - - - - - - '



print ' - - - - - - - - - - - - - - - - - - - - - - '
my_indexed_gibbs_list = zip(np.argsort(finalGibbsk), range(len(np.argsort(finalGibbsk))))
print ' my_indexed_gibbs_list = ', my_indexed_gibbs_list

min_value, min_index = min(my_indexed_gibbs_list); print ' min_value, min_index = ', min_value, min_index ; print ' for the c1 = ', c1[ min_value ], min_index
#max_value, max_index = max(my_indexed_list); print ' max_value, max_index = ', max_value, max_index ; print ' for the c1 = ', c1[ max_value ], max_index
#min_value, min_index = min(my_indexed_gibbs_list); print ' min_value, min_index = ', min_value, min_index ; print ' for the c2 = ', c2[ min_value ], min_index

print ' - - - - - - - - - - - - - - - - - - - - - - '
'''



'''
#####################################################################
pl.title(' Gibbs vs. molar fraction of componenets A and B ')

# make axis labels
pl.xlabel('x axis - molar fraction/components')
pl.ylabel('y axis - Gibbs')

component1 = pl.plot(c1,finalGibbsk, '-', label = 'comp1' )
component2 = pl.plot(c2,finalGibbsk, '--', label = 'comp2')
pl.plot(s1min2, gmin2, 'o' )
pl.plot(s2min2, gmin2, 'o' )
# set axis limits
#pl.xlim(0.0, 1.0)
#pl.ylim(0.0, 30.)
pl.legend( loc = "best" )
pl.grid()
pl.show()
'''







        


      
            


          
            
         
           
  

          
