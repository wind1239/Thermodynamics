
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT


def Activity_WilsonModel( X ):
    # Creating a temporary array that will store Gamma for each component
    Gamma = [ 0. for i in range( ThT.NComp ) ]

    for k in range( ThT.NComp ):
        sum1 = 0.
        for i in range( ThT.NComp ):
            node1 = k * ThT.NComp + i
            sum1 = sum1 + X[ i ] * ThT.Wilson_Lambda[ node1 ]

        sum2 = 0.
        for j in range( ThT.NComp ):
            sum3 = 0.
            for i in range( ThT.NComp ):
                node3 = j * ThT.NComp + i
                sum3 = sum3 + X[ i ] * ThT.Wilson_Lambda[ node3 ]
            node2 = j * ThT.NComp + k
            sum2 = sum2 + X[ j ] * ThT.Wilson_Lambda[ node2 ] / max( 1.e-7, sum3 )

        Gamma[ k ] = math.log( max( 1.e-7, 1. - math.log( sum1 ) - sum2 ) )

    return Gamma
