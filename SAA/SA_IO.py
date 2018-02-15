
#!/usr/bin/env python

import sys
import os
import numpy as np
import SA_Print as Print
import pdb

###
### Converting string to boolean
###
def to_bool(value): 
    """
       Converts 'something' to boolean. Raises exception for invalid formats
           Possible True  values: 1, True, "1", "TRue", "yes", "y", "t"
           Possible False values: 0, False, None, [], {}, "", "0", "faLse", "no", "n", "f", 0.0, ...
    """
    if str(value).lower() in ("yes", "y", "true",  "t", "1"): return True
    if str(value).lower() in ("no",  "n", "false", "f", "0", "0.0", "", "none", "[]", "{}"): return False
    raise Exception('Invalid value for boolean conversion: ' + str(value))


###
### Converting anything to wither integer or float
###            
def num(s):
    """ Convert 'something' to either float or integer """
    try:
        return int(s)
    except ValueError:
        return float(s)

###
###
###
def OutPut( Task, Method, *args, **kwargs ):
    """ Opening output file """
    global f_SAOutput

    if 'Problem_Name' in kwargs:
        Problem_Name = kwargs.get( 'Problem_Name', None )
        FileName = Task + '_' + Method + '_' + Problem_Name + '.out' # Creating file for general output
        
    elif 'Benchmark_Number' in kwargs:
        Problem_Name = kwargs.get( 'Benchmark_Number', None )
        FileName = Task + '_' + Method + '_' + 'TestCase_' + Problem_Name + '.out' # Creating file for general output

    else:
        sys.exit( 'In OutPut function. Option not found' )

    f_SAOutput = open( FileName, 'w' )

###
###
###
def CheckNumberTests():

    FileName = 'Benchmarks.in' ; ntest = 0
    SA_Cooling_list = []
    
    with open( FileName, 'r' ) as f:
        for line in f:
            if line[ 0 ] == '$': # Variable's name
                line.rstrip()
                if line.find( 'Benchmark' ) > 0:
                    ntest += 1

    f.close()
    return ntest

###
###
###
def CountingNumberOfTests( **kwargs ):
    """ This function reads the file 'Benchmarks.in' that contains the list of
            benchmark test-cases. It returns the number of test-cases and
            if a test-case number is given (through the ** argument), then
            it will also returns the name of the function associated with
            this test-case number."""
    import csv # Using csv (comma separated values) module

    FileNumber = -100 ; TestName = ''

    for key in kwargs:
        if ( key == 'Test_Case' ):
            FileNumber = kwargs[ key ]
    
    count = 0
    with open( 'Benchmarks.in' ) as file:
        reader = csv.reader( file, delimiter = ' ', skipinitialspace = True )
        for row in reader:
            if row == [] or ListOfCommentsStrings( row ): # List of comment strings that can be used
                Nothing_To_Be_Done = True
            else:
                count+=1                
                if count ==  FileNumber:
                    TestName = row[ 1 ]

    file.close()

    return count, TestName

###
###
###
def ReadInCoolingSchedule( **kwargs ):
    """ This function reads the contents of the cooling schedule file"""
    import csv # Using csv (comma separated values) module

    """ ===========================================================
            Set up Global variables used throughout the code
        ===========================================================  """
    global SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
        SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_X, BenchmarkSolution
    global SA_Xopt, SA_Fopt # These variables are defined here just for practicality


    """ Local variables: """
    SA_Cooling = [] ; SA_Cooling_list = [] ; SA_Benchmarks = []; Validation = False
    
    """Reading Input files """
    if kwargs:
        for key in kwargs:
            if ( key == 'File_Name' ): # For Problems
                SA_Function = kwargs[ key ]
                Function = SA_Function + '.sa'
                
            elif( key == 'Test_Number' ): # For Benchmark Test-Cases
                TestNumber = kwargs[ key ]
                dummy, SA_Function = CountingNumberOfTests( Test_Case = TestNumber )
                Function = os.path.abspath( './Tests/' + SA_Function + '.sa' )
                Task = 'Benchmarks'

            elif( key == 'Task' ): # For Problems
                Task = kwargs[ key ]
                
            else:
                sys.exit( 'In ReadInCoolingSchedule. Option not found' )

    else:
        sys.exit( 'Option not found' )

    Are_There_Dimensions = False
    
    """ Open input file containing Cooling Schedule: """
    with open( Function, 'r' ) as file:
        reader = csv.reader( file, delimiter = ' ', skipinitialspace = True )

        for row in reader:
            if row == [] or ListOfCommentsStrings( row ): # List of comment strings that can be used
                Nothing_To_Be_Done = True

            elif row[ 0 ] == 'Number_Dimensions': # This MUST be the first variable declared in the input file
                SA_N = int( row[ 1 ] )
                Are_There_Dimensions = True

            elif Are_There_Dimensions:

                if row[ 0 ] == 'Minimum': # Maximum or Minimum
                    SA_Minimum = to_bool( row[ 1 ] )

                elif row[ 0 ] == 'NS': # Maximum number of cycles
                    SA_NS = int( row[ 1 ] )

                elif row[ 0 ] == 'NT': # Maximum number of iterations before the temperature reduction
                    SA_NT = int( row[ 1 ] )

                elif row[ 0 ] == 'MaxEvl': # Maximum number of evaluations of the objective function
                    SA_MaxEvl = int( row[ 1 ] )

                elif row[ 0 ] == 'EPS': # Minimum acceptable discrepancy (used throughout the SAA)
                    SA_EPS = float( row[ 1 ] )

                elif row[ 0 ] == 'RT': # Parameter for temperature reduction
                    SA_RT = float( row[ 1 ] )

                elif row[ 0 ] == 'Temp': # SAA temperature parameter
                    SA_Temp = float( row[ 1 ] )

                elif row[ 0 ] == 'X_Init': # Initial guess for the solution-coordinate vector
                    SA_X = ReadingRows_Float( row )

                elif row[ 0 ] == 'LowerBounds': # Lower bounds for the solution-coordinate vector
                    SA_LowerBounds = ReadingRows_Float( row )

                elif row[ 0 ] == 'UpperBounds': # Upper bounds for the solution-coordinate vector
                    SA_UpperBounds = ReadingRows_Float( row )

                elif row[ 0 ] == 'VM': # Stepping matrix (only the diagonal representing each direction
                    SA_VM = ReadingRows_Float( row )

                elif row[ 0 ] == 'C': # Parameter for controlling the size of the stepping matrix
                    SA_C = ReadingRows_Float( row )

                elif row[ 0 ] == 'Benchmark_Solution': # Solution of the Benchmark test-case
                    if Task == 'Problem':
                        BenchmarkSolution = ReadingRows_Mix( row )
                        Validation = True
                    else:
                        BenchmarkSolution = ReadingRows_Float( row, Solution = 'yes' )

                elif row[ 0 ] == 'Debugging': # Option to dump all intermediate results into the *.out file (True or False)
                    SA_Debugging = to_bool( row[ 1 ] )

                else:
                    sys.exit('Option not recognised')

            else:
                print 'Number_Dimensions was not defined in the FIRST line'
                sys.exit()

    file.close()
    Print.Print_SAA_Diagnostic( Initialisation = 'yes' )
    
    if kwargs:
        for key in kwargs:
            if ( key == 'File_Name' ): # For Problems we assign this array as zero.
                if Validation == False:
                    BenchmarkSolution = [ 0. in range( SA_N + 1 )]
    
    return SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
        SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_X, BenchmarkSolution



def ListOfCommentsStrings( row ):
    """ This function identifies if the line is a comment."""
    list = []
    IsItaComment = False
    if (     row[ 0 ] == ''      or \
             row[ 0 ] == ' '     or \
             row[ 0 ] == '  '    or \
             row[ 0 ] == '   '   or \
             row[ 0 ] == '#'     or \
             row[ 0 ] == '##'    or \
             row[ 0 ] == '###'   or \
             row[ 0 ] == '# '    or \
             row[ 0 ] == '## '   or \
             row[ 0 ] == '### '  or \
             row[ 0 ] == '#  '   or \
             row[ 0 ] == '##  '  or \
             row[ 0 ] == '###  ' ):
        IsItaComment = True

    return IsItaComment


def ReadingRows_Float( row, *args, **kwargs ):
    """ This function reads the contents of a row and convert them into a
          real (i.e., float) array.  """

    if 'Solution' in kwargs:
        BSolution = kwargs.get( 'Solution', None )
        nd = SA_N + 1 # Solution-coordinate + Function
    else:
        nd = SA_N 

    Array = np.arange( float( nd ) )
    for i in xrange( 0, nd ):
        Array[ i ] = row[ i + 1 ]
        
    return Array

def ReadingRows_Mix( row ):
    """ This function reads the contents of a row with multiple data types. """

    Array = []
    for i in range( SA_N + 1 ):
        Array.append( row[ i + 1 ] )
        
    return Array
