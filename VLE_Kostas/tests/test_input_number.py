import numpy as np
import scipy as sc
import math
import sys

n = raw_input(' give number please? ')
n = float(n)
print
margin = raw_input(' give the +/- margins : ')
margin = float(margin)

print ' the margins are +/- ', margin
print
nlow = n - (margin * n ) ; print ' c1 ', nlow
nhigh = n + (margin * n ) ; print ' c2 ', nhigh
