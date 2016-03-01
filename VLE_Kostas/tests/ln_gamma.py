import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import time

nc = 3

x = [0.2, 0.6, 0.2]
l = [1., 4.5, 3.8, 0.5, 1., 2.5, 8.6, 2.5, 1. ]
suma = 0
sumc = 0
sumc_nom = 0
sumc_denom = 0
ln_gamma = 0


for iphase in range(1):
    print ' ---------------------------------------------------------- '
    print '                       iphase = ', iphase
    print ' ---------------------------------------------------------- '
    for k in range(nc):
        # this is the first loop for the suma
        suma = 0                                               # I do not want to tranfer the previous sum to the next iteration/node i
        for i in range(nc):                                    # so everytime, for every new loop, i set the sum a = 0  
            node = k * nc + i
            print ' k, i, node:', k, i, node, x[i],l[node]
            suma = suma + x[i] * l[node]
        print '                              the ln(suma) = ', np.log(suma) 
        print
        '''
        # this is the second loop for the sumc term
        for j in range(nc):
            node1 = k *nc + j
            sumc_denom = 0
            for i in range(nc):
                node2 = i * nc + j
                sumc_denom = sumc_denom + x[i] * l[node2]
            print ' the sumc_denom = ', sumc_denom
            sumc = sumc + x[j] * l[node1] / sumc_denom          # or it can be done like that sumc = sumc_nom / sumc_denom
            #print ' k, j, node:', k, j, node, x[j],l[node2]
        print '                                    the sumc = ', sumc
        #ln_gamma = 1 - np.log(suma) - sumc
        #print '                                    ln_gamma = ', ln_gamma 
        #print ' ---------------------- next node ----------------------- ' '''


for k in range(nc):
    for j in range(nc):
        node1 = j *nc + k 
        sumc_nom = x[j] * l[node1]
        #print ' the sumc_nom = ', sumc_nom, j, k
        sum_denomi = 0
        for i in range(nc):
            node2 = i * nc + j
            sum_denomi =  sum_denomi + x[i] * l[node2]   
            print ' the sum_denomi = ', sum_denomi, j, i
            sumc_denom = sum_denomi
        print '                             the sum_denom = ', sumc_denom
        #sumc = sumc + x[j] * l[node1] / sumc_denom 
