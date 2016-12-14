
#!/usr/bin/env python

import os, sys

lib_path = os.environ.get('OptimusPATH') + 'PhaseEquilibria'
sys.path.append( lib_path ) # <== Adding the above in the sys path for python


# Add here the Functions that will be optimised
lib_path = os.environ.get('OptimusPATH') + 'PhaseEquilibria'
#import WrapperGibbs as WpG
#import gibbsfunction_test_10 as Kostas
#import Test_A as Test 
import pdb

###
### This function 'calls' the objective function that needs to be 
###    minmised / maximised by the SAA.
###
###  REMEMBER: For the thermodynamic problem we assume that the 
###            solution-coordinate (XSolution) is:
###            X_{1}^{L}, X_{2}^{L}, ... , X_{n-1}^{L}, L
###            see documentation. 
###
def ObjFunction( XSolution, **kwargs ):

    if kwargs:
        for key in kwargs:
            if key == 'Thermodynamics':
                ProbCase = kwargs[ key ]
            elif key == 'StabilityAnalysis':
                Stability = kwargs[ key ]
            else:
                sys.exit('In ObjectiveFunction, option for problem-type was not properly defined')

        Result, Z_Feed = OptWrapper( XSolution, Thermodynamics = ProbCase )

    else:
        sys.exit('In ObjectiveFunction, option for problem-type was not properly defined (2)')


    return Result, Z_Feed
    

def OptWrapper( Ndim, XSolution, **kwargs ):

    MolarGibbs, Z_Feed = GBT.GibbsCalculation( XSolution )

    return MolarGibbs
