
#!/usr/bin/env python

import sys, os

""" This function checks if the environmental variable (EV) OptimusPATH has been set up.
       The EV OptimusPATH gives the address (i.e., pathway of the directory) of the OptimusPrime."""
def EnvirVar():
    if os.environ.get('OptimusPATH') is None:

        msg = (
            '===================================================================================================== \n'
            '  The Environmental Variable (EV) *OptimusPATH* was not defined. This can be solved by either: \n '
            '     a) Set up this EV permanently by adding the following line at the end of the .bashrc file (at the root directory): \n'
            '            export OptimusPATH="Path_To_OptimusPrime" \n'
            '        e.g., \n'
            '            export OptimusPATH="/data2/UoA/ThermodynamicModels/OptimusPrime", or; \n'
            '     b) Set up this EV temporarily by adding the following at the shell-erminal: \n'
            '            export OptimusPATH="Path_To_OptimusPrime" \n'
            '        e.g., \n'
            '            export OptimusPATH="/data2/UoA/ThermodynamicModels/OptimusPrime"  \n'
            ''
            '===================================================================================================== \n')
        print(msg)
        sys.exit( '                              **OptimusPATH** not defined.\n' )

""" This function defines the environmental paths for the main directories
    used by the code. """
def PathWays( Task, Method ):
    global RootPATH, MainPATH, OptimiserPATH, ProbPATH
    
    RootPATH = os.environ.get('OptimusPATH')
    MainPATH = RootPATH + '/Main/'

    if Task == 'Problem' :
        ProbPATH = RootPATH + '/Tests/Problems/'
    elif Task == 'Benchmarks' :
        ProbPATH = RootPATH + '/Tests/Benchmarks/'
    else:
        sys.exit('Non-existent option for PROBLEM type')

    if Method == 'SAA' or Method == 'SA':
        OptimiserPATH = RootPATH + '/SAA/'
    else:
        sys.exit('Non-existent option for METHOD option')




        
                 
        
