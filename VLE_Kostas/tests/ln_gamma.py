import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import time

nc = 2

x = [0.5, 1.]
l = [2, 4]
suma = 0
sumc = 0
sumc_nom = 0
sumc_denom = 0
ln_gamma = 0

for iphase in range(2):
    print ' ---------------------------------------------------------- '
    print ' iphase = ', iphase
    for k in range(nc):
        for i in range(nc):
            print
            suma = suma + x[i] * l[k]
            print '                                the suma = ', suma
            for j in range(nc):
                print '                                  the xj = ' , x[j]
                sumc_nom = sumc_nom + x[j] * l[i]
                print ' at the component = ', j, '     the sumc_nom = ', sumc_nom  
                for i in range(nc):
                    sumc_denom = sumc_denom + x[i] * l[j]
                    print ' at the component = ',i , j, ' the sumc_denom = ', sumc_denom 
            sumc = sumc_nom / sumc_denom
            print '                          the final sumc = ', sumc
            ln_gamma == 1 - np.log(suma) - sumc    
        print ' for iphase = ', iphase, ' the ln_gamma = ', ln_gamma 
