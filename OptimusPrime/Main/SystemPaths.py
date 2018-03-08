
#!/usr/bin/env python

import sys, os

""" This function checks if the environmental variable (EV) OptimusPATH has been set up.
       The EV OptimusPATH gives the address (i.e., pathway of the directory) of the OptimusPrime."""
def EnvirVar( Task, Method, **kwargs ):
    global RootPATH, MainPATH, OptimiserPATH, SLVE_PATH, ProbPATH
    
    if os.environ.get('OptimusPATH') is None:
        PATH = os.getcwd() ; StringMain = '/Main' ; PATH2 = PATH.strip( StringMain )
        os.environ['OptimusPATH'] = PATH2 ; RootPATH = os.environ.get('OptimusPATH')
        MainPATH = RootPATH + '/Main/'
        #MainPATH = os.getcwd() ; 
        #StringMain = '/Main' ; RootPATH = MainPATH.strip( StringMain )
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

    print RootPATH, MainPATH, OptimiserPATH

    return




        
                 
        
