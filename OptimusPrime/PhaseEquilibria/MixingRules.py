
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT
import EoS as EoS
import MixingRules_Classic as Mix_Class
import MixingRules_WongSandler as Mix_WS


'''
    THIS FILE WAS DESIGNED TO CALCULATE MIXING AND COMBINING RULES FOR EQUATIONS
         OF STATE.
'''

# Given component, pressure and temperature, this function will return
#    the mixture attractive and repulsive parameters:
def MixingRules_EoS( Temp, Press, iphase, Composition ):
    """ Composition has dimension NComp. """
    if ThT.MixingRules[ 0 ] == "Classic":
        ( FugCoeff, ChemPot ) =  Mix_Class.MixingRules_EoS_Classic( Temp, Press, iphase, Composition )
            
    elif ThT.MixingRules[ 0 ] == "Wong-Sandler":
        ( FugCoeff, ChemPot ) =  Mix_WS.MixingRules_EoS_WongSandler( Temp, Press, iphase, Composition )
        
    else:
        sys.exit( 'Mixing rules were not defined correctly!' )

    print '====::', FugCoeff, ChemPot ; sys.exit('opopop')

    return ( FugCoeff, ChemPot )
 
