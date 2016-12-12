
#!/usr/bin/env python

import sys, os

def PathWays( Task, Method ):
    global RootPATH, MainPATH, OptimiserPATH, ProbPATH
    MainPATH = os.getcwd()
    temp = os.path.split( MainPATH )
    RootPATH = temp[ 0 ]
    if Task == 'Problem' :
        ProbPATH = temp[ 0 ] + '/Tests/Problems/'
    elif Task == 'Benchmarks' :
        ProbPATH = temp[ 0 ] + '/Tests/Benchmarks/'
    else:
        sys.exit('Non-existent option for PROBLEM type')

    if Method == 'SAA':
        OptimiserPATH = temp[ 0 ] + '/SAA/'
    else:
        sys.exit('Non-existent option for METHOD option')
        
        
                 
        
