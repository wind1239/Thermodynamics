
#!/usr/bin/env python

import math
import sys, os
import numpy as np
import BenchmarkTests as BTest
import SA_IO as IO
import SA_Print as Print
import RandomGenerator as RanGen
import SpecialFunctions as SpFunc
import SAA_Tools as SaT
import ObjectiveFunction as ObF
import time
import pdb
import pylab as pl

#lib_path = os.environ.get('OptimusPATH') + '/Main/' 
#sys.path.append( lib_path ) # <== Adding the above in the sys path for python
import SystemPaths as SyP



""" =========================================================================

              FUNCTION: Starting the Simulated Annealing Algorithm

       The SAA coded here in Python was adapted from the Fortran version 
             by Goffee et al. (J. Econometrics 60(1994):65-99. And the 
             Fortran code may be obtained from:
                http://econpapers.repec.org/software/wpawuwppr/9406001.htm

    =========================================================================  """

def SimulatedAnnealing( Method, Task, **kwargs ):
      
    X_Optimum = [] 
    F_Optimum = [] 
    TestSolution = [] 
    TestSolution_Name = [] 
    TestSolution_Time = [] 
    Time_temp = []


    """
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
    
    if Task == 'Benchmarks':
        N_Tests, dummy = IO.CountingNumberOfTests( ) # Checking the total number of test-cases.
        if kwargs:
            for key in kwargs:
                if key == 'FileName' :
                    TestCases = kwargs[ key ]
                    if TestCases == 'All':
                        Test = 100 * N_Tests
                    else:
                        Test = int( TestCases )

    elif Task == 'Problem':
        if kwargs: 
            for key in kwargs:
                if key == 'FileName': 
                    ProblemFileName = kwargs[ key ]
                    Test = 0 ; N_Tests = 1
                elif key == 'Thermodynamics':
                    PhaseEquilibria = kwargs[ key ]
                    Test = 0 ; N_Tests = 1
                else:
                    sys.exit( 'In SimulatedAnnealing function. TASK-Problem option not found.' )
             
    else:
        sys.exit( 'In SimulatedAnnealing function. TASK option not found. Currently only *Benchmarks* and *Problems* are acceptable.' )

        
    """"
        =======================================================================
           Now, depending on the case, 'Benchmarks' or 'Problem' we 
                proceed the optimisation, if:
                  a) 'Benchmarks': there are 2 options here,
                      a.1 ) 'All': it will read each cooling schedule 
                                   file for all test-cases and proceed
                                   with the optimisation.
                      a.2 ) 'N': where 1 <= N <= N_Tests. It will read
                                   only the cooling schedule of test-case
                                   N and proceed with the optimisation.
                or    
                  b) 'Problem': then N_Tests = 1 and we run SAA just once
        ======================================================================= """

    jtest = 0

    for itest in xrange( 1, N_Tests + 1 ):

        if Task == 'Benchmarks':
            
            if TestCases != 'All': # Dealing with test-case N
                
                if itest == Test:
                    SaT.Function_Name, SaT.Minimum, SaT.Ndim, SaT.NS, SaT.NT, SaT.MaxEvl, SaT.EPS, SaT.RT, SaT.Temp, \
                        SaT.LowerBounds, SaT.UpperBounds, SaT.VM, SaT.C, SaT.Debugging, \
                        SaT.SA_X, SaT.BenchmarkSolution = IO.ReadInCoolingSchedule( Test_Number = Test, Task = Task )
                    print '======================', SaT.Function_Name, '======================'
                else:
                    continue
            else:
                SaT.Function_Name, SaT.Minimum, SaT.Ndim, SaT.NS, SaT.NT, SaT.MaxEvl, SaT.EPS, SaT.RT, SaT.Temp, \
                    SaT.LowerBounds, SaT.UpperBounds, SaT.VM, SaT.C, SaT.Debugging, \
                    SaT.SA_X, SaT.BenchmarkSolution = IO.ReadInCoolingSchedule( Test_Number = itest, Task = Task )
                print '======================', SaT.Function_Name, '======================'
                

        elif Task == 'Problem' or Task == 'Problems':
            SaT.Function_Name, SaT.Minimum, SaT.Ndim, SaT.NS, SaT.NT, SaT.MaxEvl, SaT.EPS, SaT.RT, SaT.Temp, \
                SaT.LowerBounds, SaT.UpperBounds, SaT.VM, SaT.C, SaT.Debugging, \
                SaT.SA_X, SaT.BenchmarkSolution = IO.ReadInCoolingSchedule( File_Name = ProblemFileName, Task = Task )

        else:
            sys.exit( 'In SimulatedAnnealing. Option not found' )
       


        """
            ===================================================================
                      Initial Assessement of the Objective Function
            ===================================================================
        """

        """ Calling the function for the first time before the SA main loop """
        if Task == 'Benchmarks':
            Func = BTest.TestFunction( SaT.Function_Name, SaT.Ndim, SaT.SA_X ) 
            
        else: # Problems
            Func, X_Feed = ObF.ObjFunction( SaT.SA_X, Thermodynamics = PhaseEquilibria, Status = 'InitialCalculations' )

        """ The function must be minimum """
        if SaT.Minimum:
            Func = -Func

        """"                  Printing into the *out file                    """
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( 'Initial evaluation of the function: {a:.4e}'.format( a = Func ) + '\n' )
        if Task == 'Problem':
            SpFunc.CalcOtherPhase( SaT.SA_X, SaT.UpperBounds, SaT.LowerBounds, Diagnostics = True )XXXXXXXX



        """
            ===================================================================
                Calling main SA loop:       
            ===================================================================
        """

        if Task == 'Benchmarks':
            X_OPT, F_OPT = ASA_Loops( Task, Func )
        else: # Problems
            X_OPT, F_OPT = ASA_Loops( Task, Func, X_Feed = X_Feed )
        
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
            Time_temp.append( TestTime )
            if TestCases == 'All' :
                if itest > 1:
                    TestSolution_Time.append( TestTime - Time_temp[ itest - 2 ] )
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
            
        else: # Problem
            TestTime = time.clock()
            TestSolution_Time.append( time.clock() - SaT.Time_Init ) # Measuring CPU time for the problem/test   

    if ( Task == 'Benchmarks' ) and ( TestCases == 'All' ):
        Print.Print_SAA_Diagnostic( Bench_AllTestCases = 'yes', Solution = TestSolution, Solution_Name = TestSolution_Name, Solution_Time = TestSolution_Time  )

    return X_Optimum, F_Optimum


###
### Main SA loop
###
def ASA_Loops( Task, Func, **kwargs ):
        
    """ For debugging """
    #pdb.set_trace()

    TestName = SaT.Function_Name
    if kwargs:
        for key in kwargs:
            if ( key == 'X_Feed' ):
                X_Feed = kwargs[ key ]
        

    IO.f_SAOutput.write( '\n' )
    IO.f_SAOutput.write( 'Initialising SA Algorithm for: {a:}'.format( a = TestName ) + '\n' )


    """ Initialisation of a few parameters. """
    Try = False
    NAcc = 0; Nobds = 0; NFCNEV = 0; NEps = 4
    MaxNum = 1.e20
   
    NACP = [ 0 for i in range( SaT.Ndim ) ]
    XP = [ 0. for i in range( SaT.Ndim ) ]
    FStar = [ MaxNum for i in range ( NEps ) ] ; FStar[ 0 ] = Func
    
    FOpt = Func ; XOpt = SaT.SA_X ; X_Try = SaT.SA_X 
    XOpt_f = [ 0. for i in range( SaT.Ndim ) ]

    """ The 'Fraction' variable assess if the function to be optimised is
           a thermodynamic function (TRUE) and therefore the elements of the
           solution-coordinate needs to be bounded (0,1) and the summation
           of the N-1 elements must be smaller than 1.
        If FALSE then the above is neglected.                               """
    if Task == 'Benchmarks':
        Fraction = False
    else: #Problems 
        Fraction = True

    kloop = 0 
    """ Beginning of the main outter loop: """
    while kloop <= SaT.MaxEvl:

        NUp = 0; NRej = 0; NDown = 0; LNobds = 0


        """ Beginning of the m loop: number of iterations """
        mloop = 0
        while mloop < SaT.NT:


            """ Beginning of j loop: number of cycles"""
            jloop = 0
            while jloop < SaT.NS:


                """ Beginning of the h loop: """
                hloop = 0

                while hloop < SaT.Ndim:

                    if Fraction:
                        dim = SaT.Ndim - 1
                    else:
                        dim = SaT.Ndim

                    for i in range( SaT.Ndim ):
                        rand = RanGen.RandomNumberGenerator( SaT.Ndim )

                        if ( i == hloop ):
                            XP[ i ] = X_Try[ i ] + SaT.VM[ i ] * ( 2. * rand[ i ] - 1. ) 
                        else:
                            XP[ i ] = X_Try[ i ]

                        #pdb.set_trace()

                    """ ===========================================================
                            Feasibility Test (only for Thermod problems) -- check
                              for compositional constraints.
                        =========================================================== """
                    #if Fraction:
                    #    XP[ dim ] = X_Try[ dim ]
                        #XP[ SaT.Ndim ] = X_Try[ SaT.Ndim ]

                    if Task == 'Benchmarks':
                        SpFunc.Envelope_Constraints( XP, NDim = SaT.Ndim, LBounds = SaT.LowerBounds, UBounds = SaT.UpperBounds, TryC = Try, IsNormalised = Fraction )
                    else: # Problems
                        SpFunc.Envelope_Constraints( XP, NDim = SaT.Ndim, LBounds = SaT.LowerBounds, UBounds = SaT.UpperBounds, TryC = Try, IsNormalised = Fraction, X_Feed = X_Feed )

                    #sys.exit()
                    
                    
                    if Try:
                        LNobds += 1
                        Nobds += 1
                    """ ============================================================ """

                    if Task == 'Benchmarks':
                        FuncP = BTest.TestFunction( SaT.Function_Name, SaT.Ndim, XP )
                    else: # Problems
                        print '--====--099980'
                        FuncP, dummy = ObF.ObjFunction( XP, Thermodynamics = PhaseEquilibria )

                    print 'here we are again .... oh dear :::',  XP, FuncP
                    #sys.exit()

                        
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
                        Print.Print_SAA_Diagnostic( MaxEval = 'yes', FOpt = FOpt, XOpt = XOpt_f, NFCNEV = NFCNEV )
                        sys.exit('SAA did not converge. Too many evaluations of the function!')


                    """"
                       ==================================================================
                         The new solution-coordinate is accepted and the objective 
                            function increases.
                       =================================================================="""
                    
                    if ( FuncP > Func ):
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
                            for i in range( SaT.Ndim ):
                                XOpt_f[i] = XP[i]

                        if Task == 'Benchmarks':
                            print 'NFCNEV(', NFCNEV, '), XOpt: ', XOpt_f, ' with FOpt: ', FOpt, '(Analytical:', -BTest.TestFunction( SaT.Function_Name, SaT.Ndim, SaT.BenchmarkSolution[ 0 : SaT.Ndim ] ),')'
                        else:
                            if (NFCNEV % 25) >= 24 :
                               print 'NFCNEV(', NFCNEV, '), XOpt: ', XOpt_f, ' with FOpt: ', FOpt
                            
                            if SaT.Debugging == True :
                                IO.f_SAOutput.write( '{s:20} New XOpt: {a:} with FOpt: {b:}'.format( s = ' ', a = XOpt_f, b = FOpt ) + '\n')

                    else:
                        """ However if FuncP is smaller than the others, thus the Metropolis criteria 
                               (Gaussian probability density function) - or any other density function
                               that may be added latter - may be used to either accept or reject this
                               coordinate.                                                            """
                        
                        rand = RanGen.RandomNumberGenerator( SaT.Ndim )
                        #Density = math.exp( ( FuncP - Func ) / SaT.Temp )#max( SaT.Temp, SaT.EPS ) )
                        if SaT.Temp < max( 1.e-6, SaT.EPS ):
                            Density = math.exp( ( FuncP - Func ) / max( 1.e-3 * SaT.EPS, rand[0] ) )
                        else:
                            Density = math.exp( ( FuncP - Func ) / SaT.Temp )
                        temp = 1.
                        for i in range( SaT.Ndim ):
                            temp = temp * rand[ i ]
                        Density_Gauss = math.sqrt(abs( temp ) )

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
                    #pdb.set_trace()
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
        
        # This will make the tests run faster as we know the solution, thus they do
        #     not need to continue search if the solution if close enough
        if Task == 'Benchmarks':
            Quit = BTest.AssessTests( XOpt_f, SaT.BenchmarkSolution )
            if Quit:
                if  SaT.Minimum :
                    FOpt = -FOpt
                Print.Print_SAA_Diagnostic( Termination = 'yes', FOpt = FOpt, NRej = NRej, XOpt = XOpt_f, NFCNEV = NFCNEV )
                return XOpt_f, FOpt
        #stop         

        """
           ==========================================================
                     Checking the stoppage criteria           
           ==========================================================  """
        
        Quit = True ; FStar[ 0 ] = Func
          
        #print '--------------------------------------' 
        #print ' FOpt, FStar, Func', FOpt, FStar, Func
        #print '--------------------------------------' 

        if FOpt - FStar[ 0 ]  <= SaT.EPS :
            Quit = False 
        #print ' ===== i am here ====='
        #print ' number of dimensions ', SaT.Ndim
        for i in range( NEps ):
            if abs( Func - FStar[ i ] ) > SaT.EPS :
                Quit = False; #print ' SaT.EPS ', SaT.EPS
              
        #stop  

        if Quit:
            if Task == 'Benchmarks':
                Quit = BTest.AssessTests( XOpt_f, SaT.BenchmarkSolution )
                
            elif Task == 'Problem' and IO.to_bool( SaT.BenchmarkSolution[ 0 ] ): # For validation
                Solution = np.arange( float( SaT.Ndim ) )
                for i in range( SaT.Ndim ):
                    Solution[ i ] = IO.num( SaT.BenchmarkSolution[ i + 1 ] )

                FuncValid, dummy = ObF.ObjFunction( Solution, Thermodynamics = PhaseEquilibria )
                FuncOpt, dummy = ObF.ObjFunction( XOpt, Thermodynamics = PhaseEquilibria )
                print 'Gibbs Function (Validation, Optimum):', FuncValid, FuncOpt

                Pass = True
                if abs( FuncValid - FuncOpt ) >= SaT.EPS:
                    Pass = Pass and False

                for i in range( SaT.Ndim ):
                    if abs( XOpt[ i ] - Solution[ i ] ) >= SaT.EPS:
                        Pass = Pass and False

                if Pass:
                    print 'Error Associated with: '
                    print ' (a) Function: ', abs( FuncValid - FuncOpt ) / FuncValid * 100., '%'
                    print ' (b) Solution Variables:'
                    for i in range( SaT.Ndim ):
                        print 'X[',i,']', abs( XOpt[ i ] - Solution[ i ] ) / Solution[ i ] * 100., '%'
                    Quit == True

           

        #print '===>', SaT.BenchmarkSolution
        #print assert(SaT.BenchmarkSolution)
        #sys.exit('fck11')

        #if assert(SaT.BenchmarkSolution):
        #    sys.exit('fck11')
        #else:
        #    sys.exit('fck--')
        #elif Quit and ( Task == 'Problem' ) and 


        """
           ==========================================================
               Termination of the Simulated Annealing algorithm       
           ==========================================================  """

        if Quit:
            #X_Try = XOpt
            if  SaT.Minimum :
                FOpt = -FOpt

            Print.Print_SAA_Diagnostic( Termination = 'yes', FOpt = FOpt, NRej = NRej, XOpt = XOpt_f, NFCNEV = NFCNEV )
            if Task == 'Problem':
                SpFunc.CalcOtherPhase( XP, SaT.UpperBounds, SaT.LowerBounds, Diagnostics = True )

            return XOpt_f, FOpt
        
        #print ' XOpt_f, FOpt ', XOpt_f, FOpt

        """
           ==========================================================
              If the stoppage criteria can not be reached, then
                 continue the K-LOOP
           ==========================================================  """
        
        SaT.Temp = SaT.RT * SaT.Temp
        for i in xrange( NEps - 1, 0, -1 ):
            FStar[ i ] = FStar[ i - 1 ]
            #print 'FStar[ i ]',FStar[ i ] 

        Func = FOpt
        for i in range( SaT.Ndim ):
            X_Try[ i ] = XOpt[ i ]
            #print 'X_Try[ i ]',X_Try[ i ] 
            
        """ End of k loop """
        kloop += 1



'''
pl.title('Plot of y vs. x')  # give plot a title

pl.xlabel('x axis')          # make axis labels
pl.ylabel('y axis')

# use pylab to plot x and y
plot1 = pl.plot(FOpt, time.time(), 'r')
plot2 = pl.plot(x2, y2, 'go')

pl.legend([plot1, plot2], ('red line', 'green circles'), 'best', numpoints=1) # plot legend
pl.show() 
'''

    
