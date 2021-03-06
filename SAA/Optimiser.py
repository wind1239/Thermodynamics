
#!/usr/bin/env python

import sys
import time
import SA_IO as IO
import Simulated_Annealing as ASA
import SA_Print as Print
import SAA_Tools as SaT


###                                     ###
###              MAIN CODE              ###
###                                     ###

""" Starting code. Procedure for input deck will be changed soon.
       The command line has 2 / 3 arguments with a prescribed order:

            python Optimiser.py < Method > < Task > < Problem-File >

      where,
        1. < Method > refers to the optimisation method used. Current 
                      supported option(s) is/are: 
            "SAA": Simulated Annealing Algorithm
        
        2. < Task > refers to the optimisation problem that needs to 
                      be tackled. Current supported option(s) is/are:
            "Benchmarks": The optimisation method will be validated 
                              against test-cases contained in the 
                              "BenchmarkTests.py" file;
            "Problem":    The optimisation method will be applied to 
                              the test-case defined in the < Problem-File >.

        3. < Problem-File > refers to the test-case that the optimisation
                              method should be applied to. This argument 
                              is necessary only if < Task > = "Problem".

      Examples of use:
          a) python Optimiser.py "SAA" "Benchmarks"
               Run benchmark test-cases with the SAA;

          b) python Optimiser.py "SAA" "Problem" "Test_A"
               Optimise test-case using the SAA. The test-case, named 
                    "Test_A" is split into 3 files, 
                 i) "Test_A.saa" : contains information for the SAA's cooling schedule.
                ii) "Test_A.phys": contains information -re the physical problem.
               iii) "Test_A.py": contains the main call for the test-function.
                                                                                           """

#global Time_Init, Time_Final

# Measuring CPU-time in the beginning of the simulation:
SaT.Time_Init = time.clock()

# Input of argument:
if len( sys.argv ) < 3:
    Print.HelpInput()
    sys.exit()
    
else:
    Method = sys.argv[ 1 ]
    Task = sys.argv[ 2 ]

    if Task == 'Problem':
        ProblemFileName = sys.argv[ 3 ]
    elif Task == 'Benchmarks':
        TestCases = sys.argv[ 3 ]

""" Creating a file for general output """
if Task == 'Problem':
    IO.OutPut( Task, Method, Problem_Name = ProblemFileName )
else:
    IO.OutPut( Task, Method, Benchmark_Number = TestCases )

""" Calling the optimisation routine """

if ( Method == 'SAA' or Method == 'SA' ):
    if ( Task == 'Problem' ):
        X_Opt, F_Opt = ASA.SimulatedAnnealing( Method, Task, FileName = ProblemFileName )
    elif( Task == 'Benchmarks' ):
        X_Opt, F_Opt = ASA.SimulatedAnnealing( Method, Task, FileName = TestCases )
        
else:
    print "Method", Method, "has not been defined yet!"
    sys.exit()

    

# Measuring CPU-time at the end of the simulation and print it out:
SaT.Time_Final = time.clock()

print 'Total CPU simulation time:', SaT.Time_Final - SaT.Time_Init
IO.f_SAOutput.write(  '\n' '\n' )
IO.f_SAOutput.write( '============================================================ \n' )
IO.f_SAOutput.write( '  TOTAL CPU TIME FOR THE OPTIMISATION PROBLEM                \n' )
IO.f_SAOutput.write( '============================================================ \n' )
IO.f_SAOutput.write( '\n' )
IO.f_SAOutput.write( 'CPU Time: {a:}'.format( a = SaT.Time_Final - SaT.Time_Init ) + '\n' )
