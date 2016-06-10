
#!/usr/bin/env python

import math
import sys
import BenchmarkTests as BTest
import SA_IO as IO
import RandomGenerator as RanGen
import SpecialFunctions as SpFunc

""" =========================================================================

           Main function: Starting the Simulated Annealing Algorithm

    =========================================================================  """

def SimulatedAnnealing( Method, Task, **kwargs ):


    global Function_Name, Ndim, Minimum, NS, NT, MaxEvl, \
        EPS, RT, Temperature, LowerBounds, UpperBounds, \
        VM, C, Debugging

    X_Optimum = [] ; F_Optimum = [] ; TestSolution = [] ; N_Tests = 0


    """"
    ===================================================================
    ===================================================================
    ===================================================================

        If we are undertaken model validation through benchmarks, we
          may opt to run all benchmark test-cases or a specific one
          controlled in the original command line as:
    
              a) python Optimiser.py SAA Benchmarks All
                            or
              b) python Optimiser.py SAA Benchmarks N
    
          where N is the number of the required test-case as defined 
          in the 'Benchmarks.in' file.
    
    ===================================================================
    ===================================================================
    ===================================================================
                                                                        """
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
        sys.exit( 'In SimulatedAnnealing. Option not found' )

        
    """"
    ===================================================================
    ===================================================================
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
    
    ===================================================================
    ===================================================================
    ===================================================================
                                                                    """
    for itest in range( N_Tests ):

        if( Task == 'Benchmarks' ):
            if TestCases != 'All': # Dealing with test-case N
                if itest == Test:
                    SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
                        SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_Xopt, \
                        SA_Fopt = IO.ReadInCoolingSchedule( Test_Number = Test )
            else:
                SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
                    SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_Xopt, \
                    SA_Fopt = IO.ReadInCoolingSchedule( Test_Number = itest )
                

        elif( Task == 'Problem' ):
            #rub1 = []; rub2 = []
            SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
                SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, rub1, \
                rub2 =  IO.ReadInCoolingSchedule( File_Name = ProblemFileName )

        else:
            sys.exit( 'In SimulatedAnnealing. Option not found' )












    if( Task == 'Benchmarks' ):
        if kwargs:
            for key in kwargs:
                if ( key == 'FileName' ):
                    TestCases = kwargs[ key ]
                else:
                    sys.exit( 'In SimulatedAnnealing. Option not found' )

        else:
            sys.exit( 'In SimulatedAnnealing. Option not found' )

        SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
            SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_Xopt, \
            SA_Fopt = IO.ReadInCoolingSchedule( Test_Number = TestCases )

    elif( Task == 'Problem' ):
        if kwargs: # For Problems
            for key in kwargs:
                if ( key == 'FileName' ): 
                    ProblemFileName = kwargs[ key ]
                else:
                    sys.exit( 'In SimulatedAnnealing. Option not found' )

        else:
            sys.exit( 'In SimulatedAnnealing. Option not found' )

            rub1 = []; rub2 = []
        SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
            SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, rub1, \
            rub2 =  IO.ReadInCoolingSchedule( File_Name = ProblemFileName )

    else:
        sys.exit( 'Option not found' )

    stop

    """ Calling main SA loop: """


    for itest in range( ntests ):

        Function_Name = SA_Function[ itest ]
        Ndim = SA_N[ itest ]
        Minimum = SA_Minimum[ itest ]
        NS = SA_NS[ itest ]
        NT = SA_NT[ itest ]
        MaxEvl = SA_MaxEvl[ itest ]
        EPS = SA_EPS[ itest ]
        RT = SA_RT[ itest ]
        Temperature = SA_Temp[ itest ]
        LowerBounds = SA_LowerBounds[ itest ]
        UpperBounds = SA_UpperBounds[ itest ]
        VM = SA_VM[ itest ]
        C = SA_C[ itest ]
        Debugging = SA_Debugging[ itest ]

        if( Task == 'Benchmarks' ):
            XSolution = SA_Xopt[ itest ]
            FSolution = SA_Fopt[ itest ]

        

        """ Initial guess-solution obtained randomly """
        X_Guess = []
        X_Guess = RanGen.RandomNumberGenerator( Ndim, LowerBounds, UpperBounds )

        """ Calling the function for the first time before the SA main loop """
        Func = BTest.TestFunction( Function_Name, Ndim, X_Guess )


        X_OPT, F_OPT = ASA_Loops( Task, X_Guess, Func )
        
        X_Optimum.append( X_OPT )
        F_Optimum.append( F_OPT )

        """" Printing solutions in the screen and in the output file """

        TestSolution.append( BTest.AssessTests( Function_Name, XSolution, X_OPT, EPS ) )
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( '{a:}:{b:}'.format( a = Function_Name, b = TestSolution[ itest ] ) + '\n' )
        IO.f_SAOutput.write( '\n' )

        print Function_Name, ':', TestSolution[ itest ]


###
### Main SA loop
###

#def ASA_Loops( Task, TestName, Ndim, Minimum, NS, NT, MaxEvl, \
#                   EPS, RT, Temp, LowerBounds, UpperBounds, \
#                   VM, C, Debugging, X_Try, Func )
def ASA_Loops( Task, X_Try, Func ):
        
    """ For debugging """
    #pdb.set_trace()
    #IO.SA_GlobalVariables2()
    TestName = Function_Name
    Temp  = Temperature

    IO.f_SAOutput.write( '\n' )
    IO.f_SAOutput.write( 'Initialising SA Algorithm for: {a:}'.format( a = TestName ) + '\n' )


    """ Initialisation of a few parameters. """
    Try = False
    NAcc = 0; Nobds = 0; NFCNEV = 0; NEps = 4
    MaxNum = 1.e20
   
    NACP = [ 0 for i in range( Ndim ) ]
    XP = [ 0. for i in range( Ndim ) ]
    FStar = [ MaxNum for i in range ( NEps ) ] ; FStar[ 0 ] = Func
    
    FOpt = Func
    XOpt = X_Try

    Fraction = True


    if Task == 'Benchmarks':
        Fraction = False
    

    kloop = 0 

    """ Beginning of the main outter loop: """
    while kloop <= MaxEvl:

        NUp = 0; NRej = 0; NDown = 0; LNobds = 0


        """ Beginning of the m loop: """
        mloop = 0
        while mloop < NT:


            """ Beginning of j loop: """
            jloop = 0
            while jloop < NS:


                """ Beginning of the h loop: """
                hloop = 0
                while hloop < Ndim:

                    if Fraction:
                        dim = Ndim - 1
                    else:
                        dim = Ndim

                    for i in range( dim ):
                        rand = RanGen.RandomNumberGenerator( Ndim, LowerBounds, UpperBounds )

                        if ( i == hloop ):
                            XP[ i ] = X_Try[ i ] + VM[ i ] * ( 2. * rand[ i ] - 1. ) 
                        else:
                            XP[ i ] = X_Try[ i ]

                    if Fraction:
                        XP[ dim ] = 1. - ListSum( XP[ 0 : dim ] )
                        
                    SpFunc.Envelope_Constraints( XP, NDim = Ndim, LBounds = LowerBounds, UBounds = UpperBounds, TryC = Try, IsNormalised = Fraction )

                    if Try:
                        LNobds += 1
                        Nobds += 1

                    FuncP = BTest.TestFunction( TestName, Ndim, XP )

                    """ The function must be minimum """
                    if Minimum:
                        FuncP = -FuncP

                    NFCNEV += 1


                    """ If there were more than MAXEVL evaluations of the objective function, 
                        the SA algorithm may finish """
                    if ( NFCNEV >= MaxEvl ):
                        IO.f_SAOutput.write( '\n \n    ################################################################################## \n' )
                        IO.f_SAOutput.write( '{s:20} Maximum number of evaluations of the function was reached. Either increase MAXEVL or EPS or reduce RT or NT (NFCNEV: {a:})'.format( s = ' ', a = NFCNEV ) + '\n' )
                        IO.f_SAOutput.write( '{s:20} XOpt: {a:} with FOpt: {b:}'.format( s = ' ', a = XOpt, b = FOpt ) )
                        
                        SpFunc.CreateDummyArray( Ndim, XOpt, FOpt, EPS )
                        return XOpt, FOpt
                        #sys.exit()


                    """ The new coordinate is accepted and the objective
                        function increases """
                    if ( FuncP >= Func ):
                        X_Try = XP
                        Func = FuncP

                        if ( Debugging ):
                            IO.f_SAOutput.write( '{s:20} New vector-solution is accepted ( X: {a:}) with solution {b:.4f}'.format( s = ' ', a = X_Try, b = FuncP ) + '\n' )

                        NAcc += 1
                        NACP[ hloop ] = NACP[ hloop ] + 1
                        NUp += 1

                        """ If the new FP is larger than any other point,
                            this will be chosen as the new optimum """

                        if ( FuncP > FOpt ):
                            for i in range( Ndim ):
                                XOpt[ i ] = XP[ i ]
                            FOpt = FuncP

                    else:
                        """ However if FuncP is smaller than the others, thus the Metropolis
                            criteria (Gaussian probability density function) - or any other
                            density function that may be added latter - may be used to either
                            accept or reject this coordinate.  """
                        rand = RanGen.RandomNumberGenerator( Ndim, LowerBounds, UpperBounds )
                        Density = math.exp( ( FuncP - Func ) / max( Temp, EPS ) )
                        Density_Gauss = 0.5 * ( rand[ 0 ] * rand[ Ndim - 1 ] )

                        if ( Density_Gauss < Density ):
                            X_Try = XP
                            Func = FuncP

                            if ( Debugging ):
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

            """ As half of the evaluations may be accepted, thus the VM array may be adjusted """
            for i in range( Ndim ):
                Ratio = float( NACP[ i ] ) / float( NS )
                if ( Ratio > 0.6 ):
                    VM[ i ] = VM[ i ] * ( 1. + C[ i ] * ( Ratio - 0.6 ) / 0.4 )

                elif ( Ratio < 0.4 ):
                    VM[ i ] = VM[ i ] * ( 1. + C[ i ] * ( 0.41 - Ratio ) / 0.4 )

                if ( VM[ i ] > ( UpperBounds[ i ] - LowerBounds[ i ] ) ):
                    VM[ i ] =  UpperBounds[ i ] - LowerBounds[ i ]

                if Debugging:
                    IO.f_SAOutput.write( '{s:20} {a:3d} Points rejected. VM is adjusted to {b:}'.format( s = ' ', a = NRej, b = VM ) + '\n' )


            NACP = [ 0 for i in NACP ]
            
            """ End of m loop """
            mloop += 1

        """ Checking the stopping criteria """
        Quit = True
        FStar[ 0 ] = Func

        if ( ( FOpt - FStar[ 0 ] ) <= EPS ):
            Quit = True

        for i in range( NEps ):
            if ( abs( Func - FStar[ i ] ) > EPS ):
                Quit = False

        if Quit:
            X_Try = XOpt

            if Minimum:
                FOpt = - FOpt

            IO.f_SAOutput.write( '\n \n     ******** TERMINATION ALGORITHM *********** \n \n ' )

            IO.f_SAOutput.write( '{s:20} Minimum was found (FOpt = {a:}) with coordinates XOpt: {b:}'.format( s = ' ', a = FOpt, b = XOpt ) + '\n' )
            IO.f_SAOutput.write( '{s:20} Number of evaluations of the function:{a:5d}. Number of rejected points:{b:5d}'.format( s = ' ', a = NFCNEV, b = NRej ) + '\n' )

            return XOpt, FOpt

        """ If the stoppage criteria can not be reached, return to the LOOP """
        Temp = RT * Temp
        for i in xrange( NEps - 1, 0, -1 ):
            FStar[ i ] = FStar[ i - 1 ]

        Func = FOpt
        X_Try = XOpt
        
            
        """ End of k loop """
        kloop += 1

    
