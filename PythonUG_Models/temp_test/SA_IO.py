
#!/usr/bin/env python

import sys

###
###
def OutPut():
    """ Output: Opening output file """
    global f_SAOutput

    """ Creating file for general output """
    f_SAOutput = open( 'sa.out', 'w' )

###
###
def ReadIn( FileName ):
    """Reading Input files """

    SA_Cooling = []
    SA_Cooling_list = []
    
    SA_Benchmarks = []
    
    icount = 0
    ntest = 0
    SA_Testing = False

    with open( FileName, 'r' )

    



