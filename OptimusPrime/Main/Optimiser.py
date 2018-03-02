#!/usr/bin/env python
import sys, os
import time
import SystemPaths as SyP
#import InitCalls as IC

# Initialisation of Environmental Variables 
SyP.EnvirVar()

# Input of argument:
if len( sys.argv ) < 3:
    Print.HelpInput()
    sys.exit()
    
else:
    Method = sys.argv[ 1 ] ; Task = sys.argv[ 2 ]

    if Task == 'Problem' or Task == 'Problems':
        if len( sys.argv ) == 4: # Dealing with a thermodynamic problem
            ProblemFileName = sys.argv[ 3 ] ; PhaseEquilibria = sys.argv[ 3 ] 
        else: # dealing with any other optimisation problem
            ProblemFileName = sys.argv[ 3 ]
        
    elif Task == 'Benchmarks':
        TestCases = sys.argv[ 3 ]
        
# Creating Global Variables for directories pathways
if Task == 'Benchmarks':
    SyP.PathWays( Task, Method )
else:
    SyP.PathWays( Task, Method, Thermodynamics = PhaseEquilibria )

lib_path = os.path.abspath(SyP.OptimiserPATH) # path to SAA
sys.path.append( lib_path ) # <== Adding the above in the sys path for python
import SAA_Tools as SaT
import InitCalls as IC
SaT.Time_Init = time.clock()

# Starting Code
IC.MainCode()


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
