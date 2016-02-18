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


Aij = [0. for i in range(4**2) ]
Lamdaij = [0. for i in range(4**2) ]

sum2 = 0
sum3_1 = 0
sum3_2 = 0

for k in range( 4 ):
    for i in range( 4 ):
        for j in range( 4 ):
            print '  - - - - - - - ' 
            print ' for the activiti coef. of component ', k, ' in relation with the components ', i , j    
            node = i * 3 + j
            if i == j:
               Lamdaij[ node ] = 1
               print ' at the main diagonal, the Greek Lamda is = ', Lamdaij[ node ]
               sum3_1 = sum3_1 + MFrac[ i ] * Lamdaij[ node ]
            else:
               Lamdaij[ node ] = 0.5                                 #( V2 / V1 )  * math.exp( ( - Aij[ node ] ) / R * T ); 
               print ' the Greek Lamda is = ', Lamdaij[ node ]
               sum3_2 = sum3_2 + MFrac[ i ] * Lamdaij[ node ]
               
            print ' the sum3_1 = ', sum3_1, 'the sum3_2 = ', sum3_2, ' while the final sum3 = ', sum3_1 + sum3_2
            print




        
