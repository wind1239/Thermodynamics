
#!/usr/bin/env python

import os, sys
import numpy as np
import math

#lib_path = os.environ.get('OptimusPATH') + '/SAA'
#sys.path.append( lib_path ) # <== Adding the above in the sys path for python
#lib_path2 = os.environ.get('OptimusPATH') + '/Main'
#sys.path.append( lib_path2 ) # <== Adding the above in the sys path for python
import SAA_Tools as SaT
import SystemPaths as SyP

# =================== ASSOCIATED/EXTERNAL FUNCTIONS ====================#


###
### FUNCTION: Global Variables
###

def ReadSet_Global_Variables(): # Read variables from a external file called 'input.dat'
    import csv # Using csv (comma separated values) module

    """ ===========================================================
            Set up Global variables used throughout the code
        ===========================================================  """

    # System variables:
    global NComp, NPhase, T_System, P_System, MolarMass, Species

    # Concentration-based variables:  
    global Z_Feed, MFrac, PhaseFrac

    # Variables for the Equations of State 
    global EOS, EOS_K1, T_Crit, P_Crit, Accentric_Factor

    # Variables for the Mixing Rules:    
    global MixingRules, BinaryParameter

    # Variables for the Activity model:
    global Activity_Model, Wilson_Lambda

    # Extra variables:
    global Debug, RConst, Residual

    RConst = 8.314 # Gas constant [J/(mol.K)]
    Residual = 1.e-10
    Debug = False

    ''' The first line MUST contain the number of components that will help to build up
        all numpy arrays thus, '''

    Are_There_Components = False

    ''' Open a file named 'input.dat' that contains all thermo-physical parameters for
        the model '''
    Filename = SyP.ProbPATH + SaT.Function_Name + '.dat'
    with open( Filename, 'rt' ) as file:
        reader = csv.reader( file, delimiter = ' ', skipinitialspace = True )
        
        for row in reader:
            if row == [] or ListOfCommentsStrings( row ): # List of comment strings that can be used
                Nothing_To_Be_Done = True

            elif row[ 0 ] == 'Number_Components': # This MUST be the first variable declared in the input file
                NComp = int( row[ 1 ] )
                Are_There_Components = True

            elif Are_There_Components:
                if row[ 0 ] == 'Number_Phases': # Number of phases
                    NPhase = int( row[ 1 ] )
    #
                elif row[ 0 ] == 'System_Temperature': # Temperature of the mixture (in K)
                    T_System = Reading_MixtureConditions_Float( row )
    #
                elif row[ 0 ] == 'System_Pressure': # Pressure of the mixture (in bar)
                    P_System = Reading_MixtureConditions_Float( row )
    #
                elif row[ 0 ] == 'Crit_Temp': # Critical temperature (in K)
                    T_Crit = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Crit_Pres': # Critical pressure (in Pa)
                    P_Crit = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Molar_Mass': # Molar mass (in g/gmol)
                    MolarMass = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Components':
                    Species = ReadingRows_String( row )
    #
                elif row[ 0 ] == 'Accentric_Factor':
                    Accentric_Factor = ReadingRows_Float( row )
    #
                elif row[ 0 ] == 'Feed_Composition':
                    Z_Feed = ReadingRows_Float( row )
                    if ( abs( math.fsum( Z_Feed ) - 1. ) >= Residual ):
                        print 'Summation of Compositions is ', math.fsum( Z_Feed ), \
                            ' and it should be 1.0'
                        sys.exit()
    #
                elif row[ 0 ] == 'BinaryInteraction':
                    BinaryParameter = ReadingRealMatrix( reader )
    #
    #            """ Here we are reading the EOS and if the choice is PRSV, then we also
    #                        read the K1 parameter.                                       """
                elif row[ 0 ] == 'Equations_of_State':
                    if row[ 1 ] == 'All':
                        EOS = []; EOS_K1 = []
                        EOS.append( row[ 2 ] )
                        if EOS[ 0 ] == "Peng-Robinson-Stryjek-Vera" or \
                                EOS[ 0 ] == "PRSV":
                            EOS_K1.append( float( row[ 3 ] ) )
                        for i in range(1, NComp ):
                            EOS.append( EOS[ i - 1 ] )
                            #EOS_K1.append( float( EOS_K1[ i - 1 ] ) )
                    else:
                        EOS, EOS_K1 = ReadingEOS( reader )
    #
                elif row[ 0 ] == 'Mixing_Rules':
                    MixingRules = ReadingMixRules( row )
    #
                elif row[ 0 ] == 'MFrac':
                    MFrac = ReadingRows_Float( row, optional = 'MFrac' )
                    for i in range( NPhase ):
                        Sum2One( 'Mole Fraction' + ' ' + str( i ), \
                                     MFrac[ i * NComp : i * NComp + NComp ] )
    #
                elif row[ 0 ] == 'PhaseFrac':
                    PhaseFrac = ReadingRows_Float( row, optional = 'PhaseFrac' )
                    Sum2One( 'Phase Fraction', PhaseFrac )
    #
                elif row[ 0 ] == 'ActivityModel':
                    if row[ 1 ] == 'All':
                        Activity_Model = []
                        Activity_Model.append( row[ 2 ] )
                        if Activity_Model[ 0 ] == 'Wilson_Model':
                            Wilson_Lambda = ReadingRealMatrix( reader )
                        else:
                            sys,exit('Activity Model not implemented yet')
                    else:
                        sys,exit('Activity Model not implemented yet++')
    #
                elif row[ 0 ] == 'Debugging':
                    Debug = row[ 1 ]
    #
            else:
                sys.exit('In function ReadSet_Global_Variables. Number_components was not defined in the FIRST line')
                

# This function reads a row containing float elements
def ReadingRows_Float( row, *positional_parameters, **keyword_parameters ):
    
    if 'optional' in keyword_parameters:
        if keyword_parameters[ 'optional' ] == 'MFrac':
            Array = np.arange( float( NComp * NPhase ) )
            ndim = NComp * NPhase
        elif keyword_parameters[ 'optional' ] == 'PhaseFrac':
            Array = np.arange( float( NPhase ) )
            ndim = NPhase
    else:
        Array = np.arange( float( NComp ) )
        ndim = NComp

    for i in xrange( 0, ndim ):
        Array[ i ] = row[ i + 1 ]


    return Array

# This function reads a row containing float elements
#    Line should contain:
#       <name>  <initial>  <final>  <increment>
#    where <name> = System_Temperature or System_Pressure,
#    <final> and <increment> are used if we want to loop over temperature and/or pressure
#    if we dont want to loop over the range, just keep <increment> = 0.0
#    Also <initial> < <final> 
def Reading_MixtureConditions_Float( row ):
    Array = np.arange( float( 3 ) )
    for i in xrange( 0, 3 ):
        Array[ i ] = row[ i + 1 ]

    if Array[ 0 ] > Array[ 1 ] or Array[ 0 ] < Array[ 2 ] or Array[ 1 ] < Array[ 2 ]:
        print 'There is something wrong with ', row[ 0 ], 'field value.'
        sys.exit()
        
    return Array

# This function reads a row containing characters elements
def ReadingRows_String( row ):
    Array = []
    Array.extend( row[ 1 : NComp + 1 ] )
    
    return Array

# This function populates the binary interaction parameter (Kij) tensor in which Kij = Kji 
#    and Kij = 0 (for i=1,NComp and j=1,NComp). Kij is stored as an array Kij[ node ] with
#                             node = i * NComp + j
#    For 3 components:
#           K11   K12   K13
#           K21   K22   K23
#           K31   K32   K33
#    In this case, K11 = K22 = K33 = 0 and
#                  K12 = K21, K13 = K31 and K23 = K32
#
def ReadingBinaryParameters( reader ):
    Kij = [ 0. for i in range( NComp ** 2 ) ]
    Array_temp = [] 

    """ For line in reader:  Reading input file for Kij and allocating the data in a list
                Array_temp.append(line) """

    iline = 0
    for line in reader: # Reading input file for Kij and allocating the data in a list
        Array_temp.append( line )
        if iline == ( NumberOfCombinations( NComp, 2 ) - 1 ):
            break
        iline += 1

    for k in range( NumberOfCombinations( NComp, 2 ) ):
        temp = Array_temp[ k ]
        i = int( temp[ 0 ] ) ; j = int( temp[ 1 ] )
        node = ( i - 1 ) * NComp + ( j - 1 )
        Kij[ node ] = float( temp[ 2 ] ) # Populating the upper triangular region

    for i in range( NComp ):
        for j in range( NComp ):
            node1 = i * NComp + j
            if i > j:
                node2 = j * NComp + i 
                Kij[ node1 ] = Kij[ node2 ] # Now populating the lower triangular region

    return Kij
        

# This function calculates the number of possible combinations. Given a sample of M items, 
#    we want to know how many subsets of N tuples can be generated as,
#              Number_of_Combination = (M!) / [(N!) * ((M-N)!) ]
def NumberOfCombinations( M, N ):
    Comb = math.factorial( M ) / ( math.factorial( N ) * math.factorial( M - N ) )
    return Comb


# This function assess if the summation of compositions (mole/mass fraction) is equal to one
def ListOfCommentsStrings( row ):
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
    

# This function checks if an normalised compositional array (e.g., mass, mole or volume fractions)
#     sums up to one
def Sum2One( ident, Array, *positional_parameters, **keyword_parameters ):

    itg = True
    if abs( math.fsum( Array ) - 1. ) >= Residual:
        print 'Array ', ident, ' is not normalised correctly:', math.fsum( Array )
        sys.exit()

    nd = np.shape( Array )
    for i in range( nd[ 0 ] ):
        if abs( Array[i] - 0. ) <= Residual:
            if 'optional' in keyword_parameters:
                if keyword_parameters[ 'optional' ] == 'NotNull':
                    itg = False
            else:
                print 'Array ', ident, ' has null elements', Array
                sys.exit() 

    return itg


# This function reads the mixing rules used for the equilibrium calculations
def ReadingMixRules( row ):
    Array = []
    Array.extend( row[ 1 : NComp + 1 ] )
    
    return Array

# This function reads the equation of state used by each component:
def ReadingEOS( reader ):
    Array_temp1 = []
    Array_temp2 = []

    iline = 0
    for line in reader: # Reading input file for the list of EOS
        Array_temp1.append( line[ 1 ] )
        if Array_temp1[ iline ] == "Peng-Robinson-Stryjek-Vera" or\
                Array_temp1[ iline ] == "PRSV":
            Array_temp2.append( float( line[ 2 ] ) )
        else:
            Array_temp2.append('')
        if iline == ( NComp - 1 ):
            break
        iline += 1

    return Array_temp1, Array_temp2 

# This function reads a matrix/tensor of dimension NxN and allocates into
#    an array
def ReadingRealMatrix( reader ):
    Value = [ 0. for i in range( NComp ** 2 ) ]
    Array_temp = []

    iline = 0
    for line in reader:
        Array_temp.append( line )
        if iline == ( NComp - 1 ):
            break
        iline += 1

    for k in range( NComp ):
        temp = Array_temp[ k ]
        for i in range( NComp ):
            node = k * NComp + i
            Value[ node ] = float( temp[ i ] )

    return Value
            
