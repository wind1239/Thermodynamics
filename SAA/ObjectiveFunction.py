
#!/usr/bin/env python

import math 
import sys

# Add here the Functions that will be optimised
import Test_A as Test 

###
### This function 'calls' the objective function that needs to be 
###    minmised / maximised by the SAA.
###
###  REMEMBER: For the thermodynamic problem we assume that the 
###            solution-coordinate (XSolution) is:
###            X_{1}^{L}, X_{2}^{L}, ... , X_{n-1}^{L}, L
###            see documentation. 
###
def ObjFunction( Function_Name, Ndim, XSolution ):

    if TestName == 'test1':
        Result = Test.test1( Ndim, XSolution )

    else:
        print '====> ', TestName, ' <===='
        sys.exit( 'Objective Function not found' )

    return Result
    
