
#!/usr/bin/env python

import sys
import SA_IO as IO
import AdaptiveSimulatedAnnealing as ASA
import SA_Print as SAPrint


###                                     ###
###              MAIN CODE              ###
###                                     ###

""" Starting code. Procedure for input deck will be changed
    soon. """

# Input of argument:
if len( sys.argv ) < 3:
    SAPrint.HelpInput()
    sys.exit()
    
else:
    Method = sys.argv[ 1 ]
    Task = sys.argv[ 2 ]

    if Task == 'Problem':
        ProblemFileName = sys.argv[ 3 ]

""" Creating a file for general output """
IO.OutPut( Task, Method )



""" Generating X_Opt array """
X_Opt = []

""" Calling the optimisation routine """

if ( Method == 'SA' ):
    if ( Task == 'Problem' ):
        X_Opt, F_Opt = ASA.SimulatedAnnealing( Method, Task, FileName = ProblemFileName )
    elif( Task == 'Benchmarks' ):
        X_Opt, F_Opt = ASA.SimulatedAnnealing( Method, Task )
        
else:
    print "Method", Method, "has not been defined yet!"
    sys.exit()

    
print 'X :', X_Opt, F_Opt

