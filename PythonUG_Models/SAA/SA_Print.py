
#!/usr/bin/env python

import sys
import SA_IO as IO
import SAA_Tools as SaT
import time


###
###
###
def HelpInput():
    print ' '
    print 'Missing argument, command line should be:'
    print ' '
    print "python Optimiser.py <Method = 'SAA'> <Task = 'Benchmarks'> <Test = 'TestCases' >"
    print "              OR  "
    print "python Optimiser.py <Method = 'SAA'>  <Task = 'Problem'> <File Name >"
    print ' '
    print "e.g., python Optimiser.py SA Benchmarks all"
    print "e.g., python Optimiser.py SA Benchmarks 2"
    print "      python Optimiser.py SA Problem    VLE_MethanePentane "
    print ' '
    print " Info for the benchmark test-cases must be contained in the file"
    print " Benchmarks.in"
    print ' '

###
###
###
def Print_SAA_Diagnostic( **kwargs ):
    # This function writes into a file all relevant info -re SAA calculations

    """ ============================================================================================
                                           INITIALISATION
        ============================================================================================"""

    if kwargs.get( 'Initialisation', None ) == 'yes':
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '   Initialisation of the Simulated Annealing Algorithm:      \n' )
        IO.f_SAOutput.write( '      Test Name: {a:}'.format( a = str( IO.SA_Function ) ) +      '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '\n' )
    
        IO.f_SAOutput.write( 'Minimisation: {a:>10s}'.format( a = str( IO.SA_Minimum ) ) + '\n' )
        IO.f_SAOutput.write( 'Dimension-Space: {a:4d}'.format( a = IO.SA_N ) + '\n' )
        IO.f_SAOutput.write( 'Maximum Number of Function Evaluations: {a:4d}'.format( a = int( IO.SA_MaxEvl ) ) + '\n' )
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( 'NS: {a:3d}, NT: {b:3d}'.format( a = IO.SA_NS, b = IO.SA_NT ) + '\n' )
        IO.f_SAOutput.write( 'EPS: {a:.4e}'.format( a = IO.SA_EPS ) + '\n' )
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( 'Temperature: {a:.4f} \nParameter for temperature reduction (RT): {b:.4f}'.format( a = IO.SA_Temp, b = IO.SA_RT ) + '\n' )
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( 'Lower Bounds: {a:}, Upper Bounds: {b:}'.format( a = IO.SA_LowerBounds, b = IO.SA_UpperBounds ) + '\n' )
        IO.f_SAOutput.write( 'VM: {a:}, C: {b:}'.format( a = IO.SA_VM, b = IO.SA_C ) + '\n' )
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( 'Inital Solution for Test: {a:}'.format( a = str( IO.SA_X ) + '\n' ) )

    """ ============================================================================================
                             DIAGNOSTICS OF THE Simulated Annealing algorithm
        ============================================================================================ """

    if kwargs.get( 'Diagnostics', None ) == 'yes':
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '        Simulated Annealing algorithm diagnostics:           \n' )
        IO.f_SAOutput.write( '  (intermediate solutions before temperature reduction)      \n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '\n' )

        if SaT.Minimum:        
            
            IO.f_SAOutput.write( 'Current Temperature: {a:4e}'.format( a = SaT.Temp ) + '\n' )
            
            FuncOpt = kwargs.get( 'FOpt', None ) ; FuncOpt = -FuncOpt
            IO.f_SAOutput.write( 'Extremum value of the function: {a:}'.format( a = FuncOpt ) + '\n' )

            NUP = kwargs.get( 'NUp', None ) ; NDOWN = kwargs.get( 'NDown', None ) 
            IO.f_SAOutput.write( 'Downward/Upward movements: {a:4d}, {b:4d}'.format( a = NUP, b = NDOWN ) + '\n' )

            NREJ = kwargs.get( 'NRej', None ) ;  NACC = kwargs.get( 'NAcc', None ) 
            IO.f_SAOutput.write( 'Number of rejected /accepted points: {a:4d}, {b:4d}'.format( a = NREJ, b = NACC ) + '\n' )

            LNOBDS = kwargs.get( 'LNobds', None ) 
            IO.f_SAOutput.write( 'Number of points out of the bounds (feasibility test): {a:4d}'.format( a = LNOBDS ) + '\n' )

            Total_Number_of_Movements = NUP + NDOWN + NREJ
            IO.f_SAOutput.write( 'Total number of movements: {a:4d}'.format( a = Total_Number_of_Movements ) + '\n' )
            NFCNEV = kwargs.get( 'NFCNEV', None )
            IO.f_SAOutput.write( 'Total number of evaluations of the function: {a:4d}'.format( a = NFCNEV ) + '\n' )

            XOPT = kwargs.get( 'XOpt', None )
            IO.f_SAOutput.write( 'Current optimal solution-coordinate: {a:}'.format( a = str( XOPT ) + '\n' ) )

            FStar = kwargs.get( 'FStar', None )
            IO.f_SAOutput.write( 'FStar List: : {a:}'.format( a = str( FStar ) + '\n' ) )

        else:
            
            IO.f_SAOutput.write( 'Current Temperature: {a:4e}'.format( a = SaT.Temp ) + '\n' )
        
            FuncOpt = kwargs.get( 'FOpt', None ) 
            IO.f_SAOutput.write( 'Extremum value of the function: {a:}'.format( a = FuncOpt ) + '\n' )

            NUP = kwargs.get( 'Nup', None ) ; NDOWN = kwargs.get( 'NDown', None ) 
            IO.f_SAOutput.write( 'Upward/Downward movements: {a:4d}, {b:4d}'.format( a = NUP, b = NDOWN ) + '\n' )

            NREJ = kwargs.get( 'NRej', None ) ;  NACC = kwargs.get( 'NAcc', None ) 
            IO.f_SAOutput.write( 'Number of rejected /accepted points: {a:4d}, {b:4d}'.format( a = NREJ, b = NACC ) + '\n' )

            LNOBDS = kwargs.get( 'LNobds', None ) 
            IO.f_SAOutput.write( 'Number of points out of the bounds (feasibility test): {a:4d}'.format( a = LNOBDS ) + '\n' )

            Total_Number_of_Movements = NUP + NDOWN + NREJ
            IO.f_SAOutput.write( 'Total number of movements: {a:4d}'.format( a = Total_Number_of_Movements ) + '\n' )
            NFCNEV = kwargs.get( 'NFCNEV', None )
            IO.f_SAOutput.write( 'Total number of evaluations of the function: {a:4d}'.format( a = NFCNEV ) + '\n' )

            XOPT = kwargs.get( 'XOpt', None )
            IO.f_SAOutput.write( 'Current optimal solution-coordinate: {a:}'.format( a = str( XOPT ) + '\n' ) )

            FStar = kwargs.get( 'FStar', None )
            IO.f_SAOutput.write( 'FStar List: : {a:}'.format( a = str( FStar ) + '\n' ) )
            
            

    """ ============================================================================================
                       REACHED THE MAXIMUM NUMBER OF EVALUATIONS OF THE FUNCTIONS
        ============================================================================================ """        
        
    if kwargs.get( 'MaxEval', None ) == 'yes':            
        
        IO.f_SAOutput.write( '\n' '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '   Maximum number of evaluations of the function was reached \n' )
        IO.f_SAOutput.write( '   Either:                                                   \n' )
        IO.f_SAOutput.write( '   (a) increase MAXEVL or EPS or,                            \n' )
        IO.f_SAOutput.write( '   (b) reduce either RT or NT.                               \n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '\n' )          
        
        FuncOpt = kwargs.get( 'FOpt', None ) ; XOPT = kwargs.get( 'XOpt', None )
        NFCNEV = kwargs.get( 'NFCNEV', None )
        
        if SaT.Minimum:
            FuncOpt = -FuncOpt
        
        IO.f_SAOutput.write( 'Number of evaluations of the function (NFCNEV): {a:}'.format( s = ' ', a = NFCNEV ) + '\n' )
        IO.f_SAOutput.write( 'Current optimal solution-coordinate: {a:}'.format( a = str( XOPT ) + '\n' ) )
        IO.f_SAOutput.write( 'Current optimal value of the function: {a:}'.format( a = FuncOpt ) + '\n' )
        IO.f_SAOutput.write( '\n' )
            

    """ ============================================================================================
                                          TERMINATION OF THE ALGORITHM
        ============================================================================================ """        
        
    if kwargs.get( 'Termination', None ) == 'yes':
        
        IO.f_SAOutput.write( '\n' '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '     Termination of the Simulated Annealing algorithm:       \n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '\n' )


        FuncOpt = kwargs.get( 'FOpt', None ) ; XOPT = kwargs.get( 'XOpt', None )
        NREJ = kwargs.get( 'NRej', None ) ; NFCNEV = kwargs.get( 'NFCNEV', None )
        
        if SaT.Minimum:
            FuncOpt = -FuncOpt

        IO.f_SAOutput.write( 'Minimum was found (FOpt = {a:}) with coordinates XOpt: {b:}'.format( s = ' ', a = FuncOpt, b = XOPT ) + '\n' )
        IO.f_SAOutput.write( 'Number of evaluations of the function: {a:5d}. Number of rejected points: {b:5d}'.format( s = ' ', a = NFCNEV, b = NREJ ) + '\n' )
        IO.f_SAOutput.write( '\n' )
            

    """ ============================================================================================
                                          ASSESSING ALL BENCHMARK TEST-CASES
        ============================================================================================ """        
        
    if kwargs.get( 'Bench_AllTestCases', None ) == 'yes':
        
        IO.f_SAOutput.write( '\n' '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '     Assessment of all benchmark test-cases:       \n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '\n' )
        
        TestSolution = kwargs.get( 'Solution', None )
        TestSolution_Name = kwargs.get( 'Solution_Name', None )
        TestSolution_Time = kwargs.get( 'Solution_Time', None )

        for i in range( len( TestSolution ) ):
            IO.f_SAOutput.write( '{a:}: {b:}'.format( a = TestSolution_Name[ i ], b = TestSolution[ i ] ) + '\n' )
            IO.f_SAOutput.write( 'CPU time: {a:}'.format( s = ' ', a = TestSolution_Time[ i ] ) + '\n' )


        FinalTime = time.clock()
        IO.f_SAOutput.write( 'Total CPU time for Benchmark tests: {a:}'.format( s = ' ', a = FinalTime - SaT.Time_Init ) + '\n' )

        
        


        



