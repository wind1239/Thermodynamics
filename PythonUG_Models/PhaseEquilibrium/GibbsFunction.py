
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import PhaseStability as Michaelsen


def GibbsObjectiveFunction( InitialAssessment, Temp, Press, Comp_Phase ):

    if ThT.NPhase > 2:
        print 'This function was hacked to work only on a 2-phases system, it thus needs to be generalised.'
        sys.exit()

    Alpha = Michaelsen.AlphaPhases() # Phase distribution
    if InitialAssessment:
        if Alpha_Phase[ 1 ] > Alpha_Phase[ 0 ]:
            iphase = 0 # Vapour
        else:
            iphase = 1 # Liquid

        GibbsFunct = Calculating_Gibbs( Temp, Press, Comp_Phase, iphase )
        


    
    

    return GibbsMolar
    



    
