
#!/usr/bin/env python

import sys

###
###
def OutPut( Task, Method ):
    """ Output: Opening output file """
    global f_SAOutput

    """ Creating file for general output """
    FileName = Task + '_' + Method + '.out'
    f_SAOutput = open( FileName, 'w' )

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

    



