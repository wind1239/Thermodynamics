#!/usr/bin/env python

import sys, os
import time
import SystemPaths as SyP
lib_path = os.path.abspath(SyP.OptimiserPATH) # path to SAA
sys.path.append( lib_path ) # <== Adding the above in the sys path for python
import SA_IO as IO
import Simulated_Annealing as ASA
import SA_Print as Print
import SAA_Tools as SaT


def MainCode():

    SaT.Time_Init = time.clock()


    """ Creating a file for general output """
    if Task == 'Problem' or Task == 'Problems':
        IO.OutPut( Task, Method, Problem_Name = ProblemFileName )
    else:
        IO.OutPut( Task, Method, Benchmark_Number = TestCases )




    # Measuring CPU-time at the end of the simulation and print it out:
    SaT.Time_Final = time.clock()

    print 'Total CPU simulation time:', SaT.Time_Final - SaT.Time_Init
    IO.f_SAOutput.write(  '\n' '\n' )
    IO.f_SAOutput.write( '============================================================ \n' )
    IO.f_SAOutput.write( '  TOTAL CPU TIME FOR THE OPTIMISATION PROBLEM                \n' )
    IO.f_SAOutput.write( '============================================================ \n' )
    IO.f_SAOutput.write( '\n' )
    IO.f_SAOutput.write( 'CPU Time: {a:}'.format( a = SaT.Time_Final - SaT.Time_Init ) + '\n' )
