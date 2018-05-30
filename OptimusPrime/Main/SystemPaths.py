
#!/usr/bin/env python

import sys, os

""" This function checks if the environmental variable (EV) OptimusPATH has been set up.
       The EV OptimusPATH gives the address (i.e., pathway of the directory) of the OptimusPrime."""
def EnvirVar( Task, Method, **kwargs ):
    global RootPATH, MainPATH, OptimiserPATH, SLVE_PATH, ProbPATH
    
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
        
        sys.exit( msg )
        
    else:
        RootPATH = os.environ.get('OptimusPATH')
        MainPATH = RootPATH + '/Main/'
        sys.path.append( RootPATH ); sys.path.append( MainPATH )

        if Task == 'Problem' :
            ProbPATH = RootPATH + '/Tests/Problems/'
        elif Task == 'Benchmarks' :
            ProbPATH = RootPATH + '/Tests/Benchmarks/'
        else:
            sys.exit('Non-existent option for PROBLEM type')
        sys.path.append( ProbPATH )

        if Method == 'SAA' or Method == 'SA':
            OptimiserPATH = RootPATH + '/SAA/'
        else:
            sys.exit('Non-existent option for METHOD option')
        sys.path.append( OptimiserPATH )

        if kwargs:
            for key in kwargs:
                if ( key == 'Thermodynamics' ):
                    SLVE_PATH = RootPATH + '/PhaseEquilibria/'
                    sys.path.append( SLVE_PATH )

    return




        
                 
        
