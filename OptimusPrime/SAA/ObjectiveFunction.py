#!/usr/bin/env python

import os, sys
import SystemPaths as SyP

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

    ProbStatus = 'Old'
    if kwargs:
        for key in kwargs:
            if key == 'Thermodynamics':
                ProbCase = kwargs[ key ]
            elif key == 'Status':
                ProbStatus = kwargs[ key ]
            else:
                sys.exit('In ObjectiveFunction, option for problem-type was not properly defined')


        if ProbCase == 'PhaseEquilibria':
            import Main_Thermod as MTh
            Result, Z_Feed = MTh.WrapThermodynamics( XSolution, Status = ProbStatus )
        else:
            sys.exit('In ObjectiveFunction, option for problem-type was not properly defined')
            
    else:
        sys.exit('In ObjectiveFunction, option for problem-type was not properly defined (2)')


    return Result, Z_Feed
    
