
#!/usr/bin/env python

import sys
import SA_IO as IO
import AdaptiveSimulatedAnnealing as ASA



###                                     ###
###              MAIN CODE              ###
###                                     ###

""" Starting code. Procedure for input deck will be changed
    soon. """

# Input of argument:
if len( sys.argv ) < 4:
    HelpInput()
    sys.exit()
    
else:
    Method = sys.argv[ 2 ]
    Task = sys.argv[ 3 ]

    if Task == 'Problem':
        ProblemFileName = sys.argv[ 4 ]

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


###
###
###
def HelpInput():
    print ' '
    print 'Missing argument, command line should be:'
    print ' '
    print "python Optimiser.py <Method = 'SA'> <Task = 'Benchmarks'> "
    print "              OR  "
    print "python Optimiser.py <Method = SA>  <Task = 'Problem'> <File Name >"
    print ' '
    print "e.g., python Optimiser.py SA Benchmarks"
    print "      python Optimiser.py SA Problem    VLE_MethanePentane.in "
    print ' '
    print " Info for the benchmark test-cases must be contained in the file"
    print " Benchmarks.in"
    print ' '
    




def SimulatedAnnealing():
