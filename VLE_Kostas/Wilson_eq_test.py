# Applying the Wilson rule to find out the activity coef.

# 1. make 3 for loops, for k, for i, for j 
# 2. then type and calcualte each of the terms of the Wilson eq. by...
#    2a. at the second term there is Sigma summation at the denominator, where xi = MFrac[i], but first need to set the Aij & V1, V2
#    2b. then calculate the nominator and finally do the summation to get the 2nd term
#    2c. calculate the  np.log( Sigma summation by calling the ThT.Lamda[i] )


import numpy as np
import math
import sys
import ThermoTools as ThT
import EOS_PR as PR
import time





#MFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
MFrac = [ 0. for i in range( 4 ) ]
print 'the molar fraction is', MFrac

MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; MFrac[ 2 ] = 0.20 ; MFrac[ 3 ] = 0.2 # Vapour phase
#MFrac[ 4 ] = 0.10; MFrac[ 5 ] = 0.10; MFrac[ 6 ] = 0.20 ; MFrac[ 7 ] = 0.6 # Liquid phase

Lamdaij = [0. for i in range(4**2) ]

sum_denominator = 0

for i in range( 4 ):
    for j in range( 4 ): 
        print
        print ' you are a the ', i,' - ', j, ' elemenent' 
        node = i * 4 + j
        if i == j:
           Lamdaij[ node ] = 1; print ' the Greek Lamda is = ', Lamdaij[ node ]
        else:
           Lamdaij[ node ] = ( V2 / V1 )  * math.exp( ( - A[ node ] ) / R * T ); print ' the Greek Lamda is = ', Lamdaij[ node ] 
        #print ' the molar farction of component ', i, ' is ', MFrac(i) 
        sum_denominator = sum_denominator + MFrac(i)  
        
