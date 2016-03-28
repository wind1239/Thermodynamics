#!/usr/bin/env python
import numpy as np
import math
import sys

def Whot( Fred ):
  wh = 0.
  sz = np.shape(Fred)[0]

  print 'In function Whot'

  for i in range( sz ):
      print i, Fred[i]



def Calc_ChemPot( NPhase, frac ):
    #sz = np.shape(MFrac)[0]
    print ' now i am in the chempot function ' 
    
    chempot = ThT.Rconst * ThT.T_System[ 0 ] * ( np.log( ThT.P_System[ 0 ] * MFrac[ i ] ) )

Calc_ChemPot( phase, mfrac )
print ' - - - - back to the main chem_pot program - - - - '



# main code
NComp = 2 ;  NPhase = 3
Frac = [ 0. for i in range( NComp * NPhase ) ]


""" Loop over phases: """
for iphase in range( NPhase ):
    node_init = iphase * NComp ; node_final = iphase * NComp + NComp - 1
    ChemPot[ node_init:node_final ] = Calc_ChemPot( iphase, MFrac[ node_init:node_final ] )      




# Main code:
NComp = 2 ;  NPhase = 3
Frac = [ 0. for i in range( NComp * NPhase ) ]

'''
for iphase in range( NPhase ):
    for icomp in range( NComp ):
        node = iphase * NComp + icomp
        if iphase < icomp:
            Frac[ node ] = float(icomp+1) * 0.3 / 3.
        else:
            Frac[ node ] = float(iphase) * float(icomp) / 4.34 + 0.1


print 'In the main program:', Frac

Whot(Frac)
print '###################BACK TO THE MAIN PROGRAM'


Whot(Frac[2:5])
'''
