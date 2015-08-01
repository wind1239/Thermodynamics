





"""


# Input of argument:
if len( sys.argv) == 1:
    print ' '
    print 'Missing argument, command line should be:'
    print ' '
    sys.exit( "python main.py <Task = 'Benchmarks' or 'Problem'>" )
    
elif len( sys.argv ) == 2 :
    Task = sys.argv[1]
    
    """ Creating file for general output """
    IO.OutPut()

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







    """ Reading the Cooling Schedule from the sa.in file """
    ReadAll_SA()
    
    """ Checking if the initial temperature is negative """
    if ( SA_Temp <= 0. ):
        sys.exit("*** Stop! Negative SA temperature")

    """ Initialising a few key variables """
    xx = [] # xx will be obtained from the up routine
    xx_opt = []
    func = []
    func_opt = []
    fstar = []

    Fraction = True


    """ Initilising and bounding the X variable """
    if SA_Testing:
        for itest in range( len( SA_Benchmarks ) ):

            TestName, Dimension, BM_Minimum, BM_Lower_Bounds, BM_Upper_Bounds, BM_VM, BM_C, BM_Solution, BL_Optimal = ExtractingFields_BM( itest )

            
            """ This need to be modified to be obtained from the function calling """
            xx = RandomNumberGenerator( Dimension, BM_Lower_Bounds, BM_Upper_Bounds )
            Fraction = False
            Envelope_Constraints( xx , NDim = Dimension, LBounds = BM_Lower_Bounds, UBounds = BM_Upper_Bounds, IsNormalised = Fraction )

            """ Calling the objective function for the first time """
            func.append( BTest.TestFunction( TestName, Dimension, xx ) )

            """ The function must be minimum, thus, in order to avoid any
            possible mess all the signals may be changed """
            if BM_Minimum:
                func = [ - res for res in func ]

            fstar.append( func )

            print 'test:', TestName, itest, len( SA_Benchmarks )

            """ Calling the SA algorithm main loop """
            xxopt, fopt = ASA_Loops( TestName, xx, func[ itest ], NDim = Dimension, Minimum = BM_Minimum, LBounds = BM_Lower_Bounds, UBounds = BM_Upper_Bounds, VM = BM_VM, C = BM_C  )

    else:
        xx = RandomNumberGenerator( SA_N, SA_LowerBounds, SA_UpperBounds )
        Envelope_Constraints( xx )

        """ Calling the objective function for the first time """
        TestName = 'Dummy1'
        func.append( BTest.TestFunction( TestName, SA_N, xx ) )

        """ The function must be minimum, thus, in order to avoid any
        possible mess all the signals may be changed """
        if SA_Minimum:
            func = [ - res for res in func ]

        fstar.append( func )

        """ Calling the SA algorithm main loop """
        xxopt, fopt = ASA_Loops( TestName, xx, func[ itest ] )

    xx_opt = xx

    

    return xxopt, fopt


""""
