import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR


# nc = ThT.NComp
print ' the number of components = ', ThT.NComp
# ThT.MFrac[i], x = [0.2, 0.6, 0.2]
# ThT. = [1., 4.5, 3.8, 0.5, 1., 2.5, 8.6, 2.5, 1. ]
suma = 0
sumc = 0
sumc_nom = 0
sumc_denom = 0
ln_gamma = 0


def ln_gamma( MFrac , Lamda_wilson):
    for iphase in range(1):
        print ' ---------------------------------------------------------- '
        print '                       iphase = ', iphase
        print ' ---------------------------------------------------------- '
        for k in range(ThT.NComp):
            suma = 0                                               
            for i in range(ThT.NComp):                                      
                node = k * ThT.NComp + i
                #print ' k, i, node:', k, i, node, x[i],l[node]
                suma = suma + ThT.MFrac[i] * ThT.Lamda_wilson[node]
            print '                              ln(suma) = ', np.log(suma) 
            for j in range(ThT.NComp):
                node1 = j * ThT.NComp + k 
                sumc_nom = ThT.MFrac[j] * ThT.Lamda_wilson[node1]
                #print ' the sumc_nom = ', sumc_nom, j, k
                sum_denomi = 0
                sumcj = 0
                for i in range(ThT.NComp):
                    node2 = j * ThT.NComp + i
                    sum_denomi = sum_denomi + ThT.MFrac[i] * ThT.Lamda_wilson[node2]   
                    #print ' the sum_denomi = ', sum_denomi, j, i
                    sumcj = sumcj +  sumc_nom/ sum_denomi
            print '                                 sumcj = ', sumcj
            print ' for the componenent ', k , '      ln_gamma = ', 1 - np.log(suma) - sumcj
            print
        return ln_gamma

    
