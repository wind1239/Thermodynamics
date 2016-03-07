import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR



def ln_gamma( MFrac ):
    ln_gamma1 = [ 0. for i in range( ThT.NComp ) ]
    for k in range(ThT.NComp):
        suma = 0                                               
        for i in range(ThT.NComp):                                      
            node = k * ThT.NComp + i
            #print ' k, i, node:', k, i, node, x[i],l[node]
            suma = suma + MFrac[i] * ThT.Lamda_wilson[node]
        print '                               ln(suma) = ', np.log(suma) 
        sumcj = 0
        for j in range(ThT.NComp):
            node1 = j * ThT.NComp + k 
            sumc_nom = MFrac[j] * ThT.Lamda_wilson[node1]
            #print ' the sumc_nom = ', sumc_nom, j, k
            sum_denomi = 0
            for i in range(ThT.NComp):
                node2 = j * ThT.NComp + i
                sum_denomi = sum_denomi + MFrac[i] * ThT.Lamda_wilson[node2]   
                #print ' the sum_denomi = ', sum_denomi, j, i
                sumcj = sumcj +  sumc_nom/ sum_denomi
        print '                                  sumcj = ', sumcj
        ln_gamma1[k] = 1 - np.log(suma) - sumcj
        print '  for the componenent ', k , '      ln_gamma = ', ln_gamma1[k]
        print
    return ln_gamma1

    


ThT.ReadSet_Global_Variables()
nc = ThT.NComp
print '  the number of components = ', ThT.NComp

sumc = 0
sumc_nom = 0
sumc_denom = 0


MFrac = [ 0. for i in range( ThT.NComp ) ];print '  the initial molar fraction before reading from the input.dat is', MFrac

# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase


lng = [ 0. for i in range( ThT.NComp ) ]
lng = ln_gamma( MFrac )
print ' the lng =', lng
