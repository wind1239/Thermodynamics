import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import time

x = [0.5, 1.]
l = [2, 4]

for i in range(2):
    for j in range(2):
        node = i * 2 + j
        inner = 1 - np.log( np.dot( x, l ) ) - 1 
        #inner = np.einsum('i,j', x, l)
        print 'at the node = ', node, '  the inner = ', inner
