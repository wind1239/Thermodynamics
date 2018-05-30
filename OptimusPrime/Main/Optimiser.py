
#!/usr/bin/env python

import sys, os
import time
import SystemPaths as SyP

###                                     ###
###              MAIN CODE              ###
###                                     ###

""" Starting code. Procedure for input deck will be changed soon.
       The command line has 2 / 3 arguments with a prescribed order:

            python Optimiser.py < Method > < Task > < TestCases >
            python Optimiser.py < Method > < Task > < Problem Type > <Problem File >

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

          b) python Optimiser.py "SAA" "Problem" "PhaseEquilibria" "Test_A"
               Optimise test-case using the SAA. The test-case, named 
                    "Test_A" is split into 3 files, 
                 i) "Test_A.saa" : contains information for the SAA's cooling schedule.
                ii) "Test_A.phys": contains information -re the physical problem.
               iii) "Test_A.py": contains the main call for the test-function.
                                                                                           """

Time_Init = time.clock() # Measuring CPU-time in the beginning of the simulation

if len( sys.argv ) < 3: # Input of argument:
    print ' '
    print 'Missing argument, command line should be:'
    print ' '
    print "python Optimiser.py <Method = 'SAA'> <Task = 'Benchmarks'> <Test = 'TestCases' >"
    print "              OR  "
    print "python Optimiser.py <Method = 'SAA'>  <Task = 'Problem'> <Type of Problem> <File Name >"
    print ' '
    print "e.g., python Optimiser.py SAA Benchmarks all"
    print "e.g., python Optimiser.py SAA Benchmarks 2"
    print "e.g., python Optimiser.py SAA Problem PhaseEquilibria VLE_MethanePentane "
    print ' '
    print " Info for the benchmark test-cases must be contained in the file"
    print " Benchmarks.in"
    print ' '
    sys.exit()
    
else:
    Method = sys.argv[ 1 ] ; Task = sys.argv[ 2 ]

    if Task == 'Problem' or Task == 'Problems' or Task == 'problems' or Task == 'problem':
        Task = 'Problem'
    elif Task == 'Benchmarks' or Task == 'benchmarks' or Task == 'Benchmark' or Task == 'benchmark':
        Task == 'Benchmarks'
    else:
        sys.exit( 'Task is not defined' )

    if ( Method == 'SAA' or Method == 'SA' or Method == 'SimulatedAnnealing' or
         Method == 'SimulatedAnnealingAlgorithm' or Method == 'ASA' ):
        Method == 'SAA'
        
    else:
        sys.exit( 'Method is not defined' )
        

    if Task == 'Problem':
        if len( sys.argv ) == 5: # Dealing with a thermodynamic problem
            ProblemType = sys.argv[ 3 ] ; ProblemFileName = sys.argv[ 4 ]
            if ( ProblemType == 'PhaseEquilibria' or ProblemType == 'phaseequilibria' or
                 ProblemType == 'Phase_Equilibria' ):
                ProblemType = 'PhaseEquilibria'
                SyP.EnvirVar( Task, Method, Thermodynamics = ProblemType )# Creating Global Variables for directories pathways
            else:
                sys.exit('ProblemType must be defined') 
        else: # dealing with any other optimisation problem
            ProblemFileName = sys.argv[ 3 ]
            sys.exit('ProblemFileName must be defined')
        
    elif Task == 'Benchmarks':
        TestCases = sys.argv[ 3 ]
        SyP.EnvirVar( Task, Method )# Creating Global Variables for directories pathways

    else:
        sys.exit( ' Task was not properly defined' )
        

"""
                    Creating a file for general output and 
                                     Calling the optimisation routine
                                                                              """
if Method == 'SAA':
    import SA_IO as IO ; import Simulated_Annealing as ASA ; import SA_Print as Print ; \
        import SAA_Tools as SaT
    
    if Task == 'Problem':
        IO.OutPut( Task, Method, Problem_Name = ProblemFileName )
        if ProblemType == 'PhaseEquilibria':
            X_Opt, F_Opt = ASA.SimulatedAnnealing( Method, Task, FileName = ProblemFileName,
                                                   ProblemType = ProblemType )
        else:
            sys.exit( 'ProblemType was not defined' )
            
            
    elif Task == 'Benchmarks':
        IO.OutPut( Task, Method, Benchmark_Number = TestCases )
        X_Opt, F_Opt = ASA.SimulatedAnnealing( Method, Task, FileName = TestCases )
        
    else:
        sys.exit( ' Task was not properly defined' )

    Time_Final = time.clock() #  Measuring CPU-time at the end of the simulation
    print 'Total CPU simulation time:', Time_Final - Time_Init
    IO.f_SAOutput.write(  '\n' '\n' )
    IO.f_SAOutput.write( '============================================================ \n' )
    IO.f_SAOutput.write( '  TOTAL CPU TIME FOR THE OPTIMISATION PROBLEM                \n' )
    IO.f_SAOutput.write( '============================================================ \n' )
    IO.f_SAOutput.write( '\n' )
    IO.f_SAOutput.write( 'CPU Time: {a:}'.format( a = Time_Final - Time_Init ) + '\n' )
    IO.f_SAOutput.write(  '\n' '\n' )
    
else:
    print "Method", Method, "has not been defined yet!"
    sys.exit( )

