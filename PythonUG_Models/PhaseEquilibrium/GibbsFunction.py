
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS


def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max

    for i in range( ThT.NComp ):
        print 'here1:', ThT.Species[ i ], EoS.PR_EoS_Calc_a( i, Temp ), EoS.PR_EoS_Calc_b( i )

    ''' Do something here ... '''

    return GibbsEnergy
    



    
