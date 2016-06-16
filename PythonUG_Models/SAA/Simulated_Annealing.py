
#!/usr/bin/env python

import math
import sys
import numpy as np
import BenchmarkTests as BTest
import SA_IO as IO
import SA_Print as Print
import RandomGenerator as RanGen
import SpecialFunctions as SpFunc
import SAA_Tools as SaT
import ObjectiveFunction as ObF
import time

""" =========================================================================

              FUNCTION: Starting the Simulated Annealing Algorithm

       The SAA coded here in Python was adapted from the Fortran version 
             by Goffee et al. (J. Econometrics 60(1994):65-99. And the code
             may be obtained from:
                zhttp://econpapers.repec.org/software/wpawuwppr/9406001.htm

    =========================================================================  """

def SimulatedAnnealing( Method, Task, **kwargs ):

    X_Optimum = [] ; F_Optimum = [] ; TestSolution = [] ; TestSolution_Name = [] ;
    TestSolution_Time = [] ; Time_temp = []


    """"
    ===================================================================
        If we are undertaken model validation through benchmarks, we
          may opt to run all benchmark test-cases or a specific one
          controlled in the original command line as:
    
              a) python Optimiser.py SAA Benchmarks All
                            or
              b) python Optimiser.py SAA Benchmarks N
    
          respectively, where N is the number of the required test-case  
          as defined in the 'Benchmarks.in' file.   
    =================================================================== """
    
    N_Tests, dummy = IO.CountingNumberOfTests( ) # Checking the total number of test-cases.
    if( Task == 'Benchmarks' ):
        if kwargs:
            for key in kwargs:
                if ( key == 'FileName' ):
                    TestCases = kwargs[ key ]
                    if TestCases == 'All':
                        Test = 100 * N_Tests
                    else:
                        Test = int( TestCases )

    elif( Task == 'Problem' ):
        if kwargs: 
            for key in kwargs:
                if ( key == 'FileName' ): 
                    ProblemFileName = kwargs[ key ]
                    Test = 0 ; N_Tests = 0
             
    else:
        sys.exit( 'In SimulatedAnnealing function. Option not found' )

        
    """"
    ===================================================================
        Now, depending on the case, 'Benchmarks' or 'Problem' we 
          proceed the optimisation, if:
    
              a) 'Problem': then N_Tests = 0 and we run SAA just once
                            or
              b) 'Benchmarks': there are 2 options here,
                  b.1 ) 'All': it will read each cooling schedule 
                               file for all test-cases and proceed
                               with the optimisation.
                  b.2 ) 'N': where 1 <= N <= N_Tests. It will read
                             only the cooling schedule of test-case
                             N and proceed with the optimisation.
    =================================================================== """

    jtest = 0

    for itest in xrange( 1, N_Tests + 1 ):

        if( Task == 'Benchmarks' ):
            
            if TestCases != 'All': # Dealing with test-case N
                
                if itest == Test:
                    SaT.Function_Name, SaT.Minimum, SaT.Ndim, SaT.NS, SaT.NT, SaT.MaxEvl, SaT.EPS, SaT.RT, SaT.Temp, \
                        SaT.LowerBounds, SaT.UpperBounds, SaT.VM, SaT.C, SaT.Debugging, \
                        SaT.SA_X, SaT.BenchmarkSolution = IO.ReadInCoolingSchedule( Test_Number = Test )
                else:
                    continue
            else:
                SaT.Function_Name, SaT.Minimum, SaT.Ndim, SaT.NS, SaT.NT, SaT.MaxEvl, SaT.EPS, SaT.RT, SaT.Temp, \
                    SaT.LowerBounds, SaT.UpperBounds, SaT.VM, SaT.C, SaT.Debugging, \
                    SaT.SA_X, SaT.BenchmarkSolution = IO.ReadInCoolingSchedule( Test_Number = itest )
                

        elif( Task == 'Problem' ):
            SaT.Function_Name, SaT.Minimum, SaT.Ndim, SaT.NS, SaT.NT, SaT.MaxEvl, SaT.EPS, SaT.RT, SaT.Temp, \
                SaT.LowerBounds, SaT.UpperBounds, SaT.VM, SaT.C, SaT.Debugging, \
                SaT.SA_X, SaT.BenchmarkSolution = IO.ReadInCoolingSchedule( File_Name = ProblemFileName )

        else:
            sys.exit( 'In SimulatedAnnealing. Option not found' )


        """
            ===================================================================
                      Initial Assessemnt of the Objective Function
            ===================================================================
        """

        """ Calling the function for the first time before the SA main loop """
        if Task == 'Benchmarks':
            Func = BTest.TestFunction( SaT.Function_Name, SaT.Ndim, SaT.SA_X ) 
            
        else: # Problems
            Func = ObF.ObjFunction( SaT.Function_Name, SaT.Ndim, SaT.SA_X )
            
        """ The function must be minimum """
        if SaT.Minimum:
            Func = -Func

        """"                  Printing into the *out file                    """
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( 'Initial evaluation of the function: {a:.4e}'.format( a = Func ) + '\n' )
        

        """
            ===================================================================
                Calling main SA loop:       
            ===================================================================
        """      

        X_OPT, F_OPT = ASA_Loops( Task, Func )
        TestTime = time.clock()
        TestSolution_Time.append( SaT.Time_Init - time.clock() ) # Measuring CPU time for the problem/test
        
        X_Optimum.append( X_OPT )
        F_Optimum.append( F_OPT )

        """
           =====================================================================
               Assessing the solution (comparison against known solution)
           ====================================================================="""
        if Task == 'Benchmarks':
            TestSolution.append( BTest.AssessTests( X_OPT, SaT.BenchmarkSolution ) )
            TestSolution_Name.append( SaT.Function_Name )

            TestTime = time.clock()
            Time_temp.append( TestTime  )
            if TestCases == 'All' :
                if itest > 0:
                    TestSolution_Time.append( TestTime - Time_temp[ itest - 1 ] )
                else:
                    TestSolution_Time.append( TestTime )     
            
            IO.f_SAOutput.write( '\n' )
            IO.f_SAOutput.write( '===========================================================' )
            IO.f_SAOutput.write( '\n' )
            IO.f_SAOutput.write( '            Assessment of the test-cases :      ' )
            IO.f_SAOutput.write( '\n' )
            IO.f_SAOutput.write( '{a:}:  {b:}'.format( a = SaT.Function_Name, b = TestSolution[ jtest ] ) + '\n' )
            IO.f_SAOutput.write( '\n' )
            jtest += 1

    if ( Task == 'Benchmarks' ) and ( TestCases == 'All' ):
        Print.Print_SAA_Diagnostic( Bench_AllTestCases = 'yes', Solution = TestSolution, Solution_Name = TestSolution_Name, Solution_Time = TestSolution_Time  )

    #return X_OPT, F_OPT
    return X_Optimum, F_Optimum


###
### Main SA loop
###
def ASA_Loops( Task, Func ):
        
    """ For debugging """
    #pdb.set_trace()
    TestName = SaT.Function_Name

    IO.f_SAOutput.write( '\n' )
    IO.f_SAOutput.write( 'Initialising SA Algorithm for: {a:}'.format( a = TestName ) + '\n' )


    """ Initialisation of a few parameters. """
    Try = False
    NAcc = 0; Nobds = 0; NFCNEV = 0; NEps = 4
    MaxNum = 1.e20
   
    NACP = [ 0 for i in range( SaT.Ndim ) ]
    XP = [ 0. for i in range( SaT.Ndim ) ]
    FStar = [ MaxNum for i in range ( NEps ) ] ; FStar[ 0 ] = Func
    
    FOpt = Func ; XOpt = SaT.SA_X ; X_Try = SaT.SA_X ;  XOpt2 = SaT.SA_X 

    """ The 'Fraction' variable assess if the function to be optimised is
           a thermodynamic function (TRUE) and therefore the elements of the
           solution-coordinate needs to be bounded (0,1) and the summation
           of the N-1 elements must be smaller than 1.
        If FALSE then the above is neglected.                               """
    if Task == 'Benchmarks':
        Fraction = False
    else:
        Fraction = True

    kloop = 0 
    """ Beginning of the main outter loop: """
    while kloop <= SaT.MaxEvl:

        NUp = 0; NRej = 0; NDown = 0; LNobds = 0


        """ Beginning of the m loop: """
        mloop = 0
        while mloop < SaT.NT:


            """ Beginning of j loop: """
            jloop = 0
            while jloop < SaT.NS:


                """ Beginning of the h loop: """
                hloop = 0

                while hloop < SaT.Ndim:

                    if Fraction:
                        dim = SaT.Ndim - 1
                    else:
                        dim = SaT.Ndim

                    for i in range( dim ):
                        rand = RanGen.RandomNumberGenerator( dim )

                        if ( i == hloop ):
                            XP[ i ] = X_Try[ i ] + SaT.VM[ i ] * ( 2. * rand[ i ] - 1. ) 
                        else:
                            XP[ i ] = X_Try[ i ]

                    """ ===========================================================
                            Feasibility Test (only for Thermod problems) -- check
                              for compositional constraints.
                        =========================================================== """
                    if Fraction:
                        XP[ dim - 1 ] = 1. - SpFunc.ListSum( XP[ 0 : dim ] )
                        
                        
                    SpFunc.Envelope_Constraints( XP, NDim = SaT.Ndim, LBounds = SaT.LowerBounds, UBounds = SaT.UpperBounds, TryC = Try, IsNormalised = Fraction )
                    
                    if Try:
                        LNobds += 1
                        Nobds += 1
                    """ ============================================================ """

                    if Task == 'Benchmarks':
                        FuncP = BTest.TestFunction( SaT.Function_Name, SaT.Ndim, XP )
                    else: # Problems
                        FuncP = ObF.ObjFunction( SaT.Function_Name, SaT.Ndim, XP )

                        
                    """ The function must be minimum """
                    if SaT.Minimum:
                        FuncP = -FuncP

                    NFCNEV += 1 # Number of evaluation of the function


                    """
                       ==================================================================
                         If there were more than MAXEVL evaluations of the objective
                            function, the SA algorithm will finish.
                       =================================================================="""
                    
                    if ( NFCNEV >= SaT.MaxEvl ):
                        FOpt = - FOpt
                        Print.Print_SAA_Diagnostic( MaxEval = 'yes', FOpt = FOpt, XOpt = XOpt, NFCNEV = NFCNEV )
                        sys.exit('SAA did not converge. Too many evaluations of the function!')


                    """"
                       ==================================================================
                         The new solution-coordinate is accepted and the objective 
                            function increases.
                       =================================================================="""
                    
                    if ( FuncP >= Func ):
                        for i in range( SaT.Ndim ):
                            X_Try[ i ] = XP[ i ]
                        Func = FuncP

                        if SaT.Debugging == True :
                            IO.f_SAOutput.write( '{s:20} New vector-solution is accepted ( X: {a:}) with solution {b:.4f}'.format( s = ' ', a = X_Try, b = FuncP ) + '\n' )

                        NAcc += 1 
                        NACP[ hloop ] = NACP[ hloop ] + 1
                        NUp += 1

                        """ If the new FP is larger than any other point, this will be 
                               chosen as the new optimum                               """

                        if ( FuncP > FOpt ):
                            for i in range( SaT.Ndim ):
                                XOpt[ i ] = XP[ i ]
                            FOpt = FuncP
                            #np.append( XOpt2, XP, FOpt  )

                            IO.f_SAOutput.write( '{s:20} New XOpt: {a:} with FOpt: {b:}'.format( s = ' ', a = XOpt2, b = FOpt ) + '\n')
                            
                            if SaT.Debugging == True :
                                IO.f_SAOutput.write( '{s:20} New XOpt: {a:} with FOpt: {b:}'.format( s = ' ', a = XOpt, b = FOpt ) + '\n')

                    else:
                        """ However if FuncP is smaller than the others, thus the Metropolis criteria 
                               (Gaussian probability density function) - or any other density function
                               that may be added latter - may be used to either accept or reject this
                               coordinate.                                                            """
                        
                        rand = RanGen.RandomNumberGenerator( SaT.Ndim )
                        Density = math.exp( ( FuncP - Func ) / SaT.Temp )#max( SaT.Temp, SaT.EPS ) )
                        Density_Gauss = 0.5 * ( rand[ 0 ] * rand[ SaT.Ndim - 1 ] )

                        if ( Density_Gauss < Density ):
                            for i in range( SaT.Ndim ):
                                X_Try[ i ] = XP[ i ]
                            Func = FuncP

                            if SaT.Debugging == True :
                                IO.f_SAOutput.write( '\n \n ')
                                IO.f_SAOutput.write( '{s:20} Metropolis Criteria; New vector-solution is generated ( X: {a:}) with solution {b:.4f}'.format( s = ' ', a = X_Try, b = FuncP ) + '\n' )

                            NAcc += 1 
                            NACP[ hloop ] = NACP[ hloop ] + 1
                            NDown += 1

                        else:
                            NRej += 1

                    """ End of h loop """
                    hloop += 1
                    
                """ End of j loop """
                jloop += 1

            """
               ==================================================================
                 As half of the evaluations may be accepted, thus the VM array
                     may need to be adjusted.
               ================================================================== """
            
            for i in range( SaT.Ndim ):
                Ratio = float( NACP[ i ] ) / float( SaT.NS )
                if ( Ratio > 0.6 ):
                    SaT.VM[ i ] = SaT.VM[ i ] * ( 1. + SaT.C[ i ] * ( Ratio - 0.6 ) / 0.4 )

                elif ( Ratio < 0.4 ):
                    SaT.VM[ i ] = SaT.VM[ i ] / ( 1. + SaT.C[ i ] * ( 0.4 - Ratio ) / 0.4 )

                if ( SaT.VM[ i ] > ( SaT.UpperBounds[ i ] - SaT.LowerBounds[ i ] ) ):
                    SaT.VM[ i ] =  SaT.UpperBounds[ i ] - SaT.LowerBounds[ i ]

            if SaT.Debugging == True :
                IO.f_SAOutput.write( '{s:20} {a:3d} Points rejected. VM is adjusted to {b:}'.format( s = ' ', a = NRej, b = SaT.VM ) + '\n' )


            NACP = [ 0 for i in range( SaT.Ndim ) ]
            
            """ End of m loop """
            mloop += 1

        """
           =======================================================================
                Diagnostics of the algorithm before the next temperature reduction
           ======================================================================= """

        Print.Print_SAA_Diagnostic( Diagnostics = 'yes', FOpt = FOpt, NUp = NUp, NDown = NDown, NRej = NRej, NAcc = NAcc, LNobds = LNobds, NFCNEV = NFCNEV, XOpt = XOpt, FStar = FStar )

        """
           ==========================================================
                     Checking the stoppage criteria
           ==========================================================  """
        
        Quit = False ; FStar[ 0 ] = Func #; FStar[ 0 ] = FOpt

        print ' '
        print 'Opt:  FOpt:', FOpt, 'Func:', Func,'=====>', 'FStar:',  FStar
        print '      XOpt:', XOpt, '======>', 'XP:', XP
        print '      XOpt2:', XOpt2
        

        #if abs( FOpt - FStar[ 0 ] ) <= SaT.EPS :
        if FOpt - FStar[ 0 ]  <= SaT.EPS :
            Quit = True


        for i in range( NEps ):
            if abs( Func - FStar[ i ] ) > SaT.EPS :
                Quit = False

        if Quit and ( Task == 'Benchmarks' ):
            Quit = BTest.AssessTests( XOpt, SaT.BenchmarkSolution )

        """
           ==========================================================
                     Checking the end of the SAA
           ==========================================================  """

        if Quit:
            #X_Try = XOpt
            if  SaT.Minimum :
                FOpt = -FOpt

            """
               =======================================================================
                        Termination of the Simulated Annealing algorithm
               ======================================================================= """

            Print.Print_SAA_Diagnostic( Termination = 'yes', FOpt = FOpt, NRej = NRej, XOpt = XOpt, NFCNEV = NFCNEV )

            return XOpt, FOpt

        """
           ==========================================================
              If the stoppage criteria can not be reached, then
                 continue the K-LOOP
           ==========================================================  """
        
        SaT.Temp = SaT.RT * SaT.Temp
        for i in xrange( NEps - 1, 0, -1 ):
            FStar[ i ] = FStar[ i - 1 ]

        Func = FOpt
        for i in range( SaT.Ndim ):
            X_Try[ i ] = XOpt[ i ]
        
            
        """ End of k loop """
        kloop += 1

    
