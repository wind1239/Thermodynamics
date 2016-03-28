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






# Main code:
NComp = 2 ;  NPhase = 3
Frac = [ 0. for i in range( NComp * NPhase ) ]

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
