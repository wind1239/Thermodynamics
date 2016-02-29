import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import time

nc = 3

x = [0.2, 0.6, 0.2]
l = [1., 4.5, 3.8, 0.5, 1., 2.5, 8.6, 0.25, 1. ]
suma = 0
sumc = 0
sumc_nom = 0
sumc_denom = 0
ln_gamma = 0

for iphase in range(1):
    print ' ---------------------------------------------------------- '
    print ' iphase = ', iphase
    for k in range(nc):
        suma = 0
        for i in range(nc):
            node = k * nc + i
            print 'k, i, node:', k, i, node, x[i],l[node]
            print
            suma = suma + x[i] * l[node]
        print '                                the suma = ', suma
        print
# this is the second loop for the sumc term
        for j in range(nc):
            #print '                                  the xj = ' , x[j]
            node1 = k *nc + j 
            for i in range(nc):
                node2 = i * nc + j
                sumc_denom = sumc_denom + x[i] * l[node2]
            sumc = sumc + x[j] * l[node1] / sumc_denom
            #sumc = sumc_nom / sumc_denom
            print 'k, j, node:', k, j, node, x[j],l[node1]
        print '                            the sumc = ', sumc
        ln_gamma = 1 - np.log(suma) - sumc
        print '                        the ln_gamma = ', ln_gamma 
        '''for i in range(nc):
            sumc_denom = sumc_denom + x[i] * l[j]
                    print ' at the component = ',i , j, ' the sumc_denom = ', sumc_denom 
            sumc = sumc_nom / sumc_denom
            print '                          the final sumc = ', sumc
            ln_gamma == 1 - np.log(suma) - sumc    
        print ' for iphase = ', iphase, ' the ln_gamma = ', ln_gamma '''

