
#!/usr/bin/env python

import os, sys

# Add here the Functions that will be optimised
lib_path = os.path.abspath('../Thermodynamics') # <== This is the syntax for using user-defined libraries in diff directories
sys.path.append( lib_path ) # <== Adding the above in the sys path for python
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
def ObjFunction( Function_Name, Ndim, XSolution ):

    #Result, Z_Feed = WpG.Wrapper( Ndim, XSolution )

    #Result, Z_Feed = WpG.Wrapper( Ndim, XSolution )

    return Result, Z_Feed
    
