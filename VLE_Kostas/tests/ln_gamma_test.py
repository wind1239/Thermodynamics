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
        suma = 0                                               
        for i in range(nc):                                      
            node = k * nc + i
            #print ' k, i, node:', k, i, node, x[i],l[node]
            suma = suma + x[i] * l[node]
        print '                              ln(suma) = ', np.log(suma) 
        for j in range(nc):
            node1 = j *nc + k 
            sumc_nom = x[j] * l[node1]
            #print ' the sumc_nom = ', sumc_nom, j, k
            sum_denomi = 0
            sumcj = 0
            for i in range(nc):
                node2 = j * nc + i
                sum_denomi = sum_denomi + x[i] * l[node2]   
            #print ' the sum_denomi = ', sum_denomi, j, i
            sumcj = sumcj +  sumc_nom/ sum_denomi
        print '                                 sumcj = ', sumcj
        print ' for the componenent ', k , '      ln_gamma = ', 1 - np.log(suma) - sumcj
        print
