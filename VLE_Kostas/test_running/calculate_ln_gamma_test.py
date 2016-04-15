import numpy as np
import math
import sys
import thermotools_test as ThT
import calculate_EOS_PR_test as PR

# = = = = = = = = = calculate the ln_gamma = = = = = = = = =
def ln_gamma( MFrac ):
    ln_gamma1 = [ 0. for i in range( ThT.NComp ) ]
    for k in range(ThT.NComp):
        suma = 0                                               
        for i in range(ThT.NComp):                                      
            node = k * ThT.NComp + i
            #print ' k, i, node:', k, i, node, 
            suma = suma + MFrac[i] * ThT.Lamda_wilson[node]
        #print '                               ln(suma) = ', np.log(suma) 
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
        #print '                                  sumcj = ', sumcj
        ln_gamma1[k] = 1 - np.log(suma) - sumcj
        #print '  for the componenent ', k , '      ln_gamma = ', ln_gamma1[k]
        #print
    return ln_gamma1

# = = = = = = = = = Gibbs energy in excess = Helmoltz energy in excess = = = = = = = = = =
def gibbs( MFrac ): 
    gibbs1 = 0
    for j in range(ThT.NComp):
        for i in range(ThT.NComp):
            node = j * ThT.NComp + i
            #print ' the node in is = ', node
            gibbs1 = - ( ThT.Rconst * ThT.T_System[ 0 ] ) * ( gibbs1 + MFrac[ j ] * ( np.log( MFrac [ i ] * ThT.Lamda_wilson[ node ] ) ) ) 
    #print '  the Gibbs energy in excess for the mixture of ', ThT.Species[i] , ThT.Species[j], ' is Ge = ', gibbs1
    print
    return gibbs1
