
#!/usr/bin/env python

import numpy as np
import math
import ThermoTools as ThT


###========================================================================###
###========================================================================###
###                                                                        ###
###   This function will be called by the simulated annealing algorithm    ###
###   with input:                                                          ###
###   X_Vec: Array of length 2*(Nc - 1), containing compositions (i.e.,    ###
###          mole fraction) at vapour and liquid phases respectively, i.e.,### 
###          X_{i}^{V} and X_{i}^{L}, and mole fraction of the liquid      ###
###          phase (L).                                                    ###
###                                                                        ###
###   The output is:                                                       ###
###   F_Gibbs: Scalar containing the Gibbs free energy for the given       ###
###            input composition.                                          ###
###                                                                        ###
###========================================================================###
###========================================================================###


# The line below will be uncommented when this code is integrated with the 
#     Simulated Annealing Algorithm code
# def Gibbs( X_Vec, F_Gibbs )


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

# Calculate ...
