import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import pylab 
import time


'''def BART( MFrac ):
    BART1 = [ 0. for i in range( ThT.NComp ) ]
    for i in range(ThT.NComp):
        for j in range(ThT.NComp):
            node = i * ThT.NComp + j
            BART1 = ( PR.PREoS_Calc_b(i) - ( PR.PREoS_Calc_a( i, ThT.T_Crit[ i ] ) / ThT.Rconst * ThT.T_System[ 0 ] ) ) + ( PR.PREoS_Calc_b(j) - ( PR.PREoS_Calc_a( j, ThT.T_Crit[ j ] ) / ThT.Rconst * ThT.T_System[ 0 ] ) ) / 2 * ( 1 -  ThT.BinaryParameter[node] )
            #print '   bart = ', BART1, ThT.Rconst, ThT.T_System[ 0 ], ThT.BinaryParameter[node]
    print
    return BART1'''


def BART2( icomp, jcomp ):
    node = icomp * ThT.NComp + jcomp
    BART2 = 0.5 * (  PR.PREoS_Calc_b(icomp)  - PR.PREoS_Calc_a( icomp, ThT.T_System[0] ) / ( ThT.Rconst * ThT.T_System[ 0 ] ) +  PR.PREoS_Calc_b(jcomp)  - PR.PREoS_Calc_a( jcomp, ThT.T_System[0] ) / ( ThT.Rconst * ThT.T_System[ 0 ] ) ) * ( 1 -  ThT.BinaryParameter[node] )
    return BART2 

 
def Q( MFrac ):
    print ' MFrac = ', MFrac
    Q1 = 0
    #F = BART( MFrac )
    for i in range(ThT.NComp):
        for j in range(ThT.NComp):
            #print ' j = ', j
            #Q1[ i ] = Q1[ i ] + MFrac[ i ] * MFrac[ j ] * F[ node ]     # Calculating the whole F first
            Q1 = Q1 + MFrac[ i ] * MFrac[ j ] * BART2( i, j )            # Falculating F when we need
            print ' ** the Q1 = ', Q1, ' the MFrac i and j are ', MFrac[ i ], MFrac[ j ],  
    print
    return Q1


def D( MFrac ):
    c = (1 / np.sqrt(2)) * np.log( np.sqrt(2)-1 )
    D1 = 0
    for i in range(ThT.NComp):
        D1 = D1 + MFrac[ i ] * ( PR.PREoS_Calc_b( i ) - PR.PREoS_Calc_a( i, ThT.T_Crit[i] ) / ThT.Rconst * ThT.T_System[ 0 ] ) + ( lng.gibbs( MFrac ) / c * ThT.Rconst * ThT.T_System[ 0 ] )
    print
    return D1


def AM( MFrac ):
    print ' ** i am at the AM( MFrac ) function ', ThT.Rconst, ThT.T_System[ 0 ]
    print ' ** ', Q( MFrac )
    AM1 = ( ThT.Rconst * ThT.T_System[ 0 ] ) * Q( MFrac ) * D( MFrac ) / ( 1 - D( MFrac ) )
    print
    return AM1


def BM( MFrac ):
    BM1 = Q( MFrac ) / ( 1 - D( MFrac ) )
    print
    return BM1


def DQ( MFrac ):
    DQ1 = [ 0. for i in range( ThT.NComp ) ]
    for i in range(ThT.NComp):
        for j in range(ThT.NComp):
            node = i * ThT.NComp + j
            DQ1[ i ] = 2 * ( DQ1[ i ] + MFrac[ j ] * BART2( i, j ) )
    print
    return DQ1


def DD( MFrac ):
    #DD1 = [ 0. for i in range( ThT.NComp ) ]
    c = (1 / np.sqrt(2)) * np.log( np.sqrt(2)-1 )
    for i in range(ThT.NComp):
        DD1 = ( PR.PREoS_Calc_a( i, ThT.T_System[0] ) / ( PR.PREoS_Calc_b( i ) * ThT.Rconst * ThT.T_System[ 0 ] ) ) + lng.ln_gamma( MFrac ) / c
    print
    return DD1

def B( MFrac ):
    B1 = ( BM( MFrac ) * ThT.P_System[0] ) / ( ThT.Rconst * ThT.T_System[ 0 ] )
    print 
    return B1




# = = = = = = = = = # = = = = = = = = = # = = = = = = = = = # = = = = = = = = = #

'''
Rconst = 8.314 # Gas constant [J/(gmol.K)]

ThT.ReadSet_Global_Variables()

MFrac = [ 0. for i in range( ThT.NComp ) ]
print '  the initial molar fraction before reading from the input.dat is ', MFrac
# declare a vector with MFrac values - molar fraction
MFrac[ 0 ] = 0.40; MFrac[ 1 ] = 0.20; # Vapour phase
#MFrac[ 2 ] = 0.10; MFrac[ 3 ] = 0.10; # Liquid phase


bart_temp = [ 0. for i in range( ThT.NComp * ThT.NComp ) ]
for i in range(ThT.NComp):
    for j in range(ThT.NComp):
          node = i * ThT.NComp + j
          bart_temp[ node ] = BART2( i, j )

print
print '  bart_temp = ', bart_temp
 

q = [ 0. for i in range( ThT.NComp ) ]
q = Q( MFrac )
print '  Q = ', q


d = [ 0. for i in range( ThT.NComp ) ]
d = D( MFrac )
print '  D = ', d
print

c = (1 / np.sqrt(2)) * np.log( np.sqrt(2)-1 )                    # c term from the eq. 2.26
print '  c = ', c
print

bm = q / ( 1 - d) 
am = bm * d
print '  bm = ', bm
print '  am = ', am
print

dq = [ 0. for i in range( ThT.NComp ) ]
dd = [ 0. for i in range( ThT.NComp ) ]
dq = DQ( MFrac )
dd = DD( MFrac ) 
print '  dq = ', dq
print '  dd = ', dd
print 
 
B = ( bm * ThT.P_System[0] ) / ( ThT.Rconst * ThT.T_System[ 0 ] )
print '   B = ', B


zmax , zmin = PR.Cubic_PR( ThT.T_System[0], ThT.P_System[0], am, bm )
y = zmax
x = zmin
print ' zmax root for the vapour phase = ', y
print ' zmin root for the liquid phase = ', x
print
'''







        
