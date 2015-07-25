
#!/usr/bin/env python

import sys
import SA_IO as IO


""" Starting code. Procedure for input deck will be changed
    soon. """


# Input of argument:
if len( sys.argv ) < 4:
    HelpInput()
    sys.exit()
    
else:
    Task = sys.argv[ 2 ]
    Method = sys.argv[ 3 ]

    if Task == 'Problem':
        ProblemFileName = sys.argv[ 4 ]
    """ Creating a file for general output """
    IO.OutPut( Task, Method )

    """ Reading input file name """
    FileName = Task + '.in'

    if( Task == 'Benchmarks' ):
        List = ReadAll_SA( FileName, ntest )

    elif( Task == 'Problem' ):
        List = ReadAll_SA( FileName )

    else:
        print 'Wrong Task Option, comand line should be:'
        sys.exit( "python main.py <Task = 'Benchmarks' or 'Problem'>" )

else:
    print 'Wrong Task Option, comand line should be:'
    sys.exit( "python main.py <Task = 'Benchmarks' or 'Problem'>" )



X_Opt = []

X_Opt, F_Opt = SimulatedAnnealing()

print 'X :', X_Opt, F_Opt



def HelpInput():
    print ' '
    print 'Missing argument, command line should be:'
    print ' '
    print "python main.py <Task = 'Benchmarks'> <Method = 'SA'> "
    print "              OR  "
    print "python main.py <Task = 'Problem'> <Method = SA> <File Name >"
    print ' '
    print "e.g., python main.py Benchmarks SA"
    print "      python main.py Problem    SA VLE_MethanePentane.in "
    print ' '
    print " Info for the benchmark test-cases must be contained in the file"
    print " Benchmarks.in"
    print ' '
    




def SimulatedAnnealing():
