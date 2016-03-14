import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR

'''def kostas( a, b, c, d):
    e = a + b + c + d
    return e '''


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
    gibbs1 = [ 0. for i in range( ThT.NComp ) ]
    for j in range(ThT.NComp):
        node = j * ThT.NComp + i
        #print ' the node out is = ', node
        for i in range(ThT.NComp):
            #print ' the node in is = ', node
            gibbs1[ i ] = - ( ThT.Rconst * ThT.T_System[ 0 ] ) * ( gibbs1[ i ] + MFrac[ j ] * ( np.log( MFrac [ i ] * ThT.Lamda_wilson[ node ] ) ) ) 
            #print '  the Gibbs energy in excess for the component ', ThT.Species[i] ,' is Ge = ', gibbs1[ i ]
        print
        return gibbs1



# = = = = = = = = = # = = = = = = = = = # = = = = = = = = = # = = = = = = = = = #

ThT.ReadSet_Global_Variables()
nc = ThT.NComp
print '  the number of components = ', ThT.NComp


MFrac = [ 0. for i in range( ThT.NComp ) ]
print '  the initial molar fraction before reading from the input.dat is', MFrac

# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase

sumc = 0
sumc_nom = 0
sumc_denom = 0

lng = [ 0. for i in range( ThT.NComp ) ]
lng = ln_gamma( MFrac )
print '  the lng =', lng 

ge = [ 0. for i in range( ThT.NComp ) ]
ge = gibbs( MFrac )
print ' the Gibbs or Helmholtz energy in excess = ', ge
