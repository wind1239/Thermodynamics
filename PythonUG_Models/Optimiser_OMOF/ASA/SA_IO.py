
#!/usr/bin/env python

import sys


def SA_GlobalVariables():
    
    global SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
        SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_Xopt, SA_Fopt, \
        X_Optimum, F_Optimum

    
    SA_Function = [] ; SA_Minimum = [] ; SA_N = [] ; SA_NS = [] ; SA_NT = [] ; \
        SA_MaxEvl = [] ; SA_EPS = [] ; SA_RT = [] ; SA_Temp = [] ; \
        SA_LowerBounds = [] ;  SA_UpperBounds = [] ; SA_VM = [] ; SA_C = [] ; \
        SA_Debugging = [] ; SA_Xopt = [] ; SA_Fopt = [] ; \
        X_Optimum = [] ; F_Optimum = []


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
def OutPut( Task, Method ):
    """ Output: Opening output file """
    global f_SAOutput

    """ Creating file for general output """
    FileName = Task + '_' + Method + '.out'
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
def ReadInCoolingSchedule( **kwargs ):

    SA_GlobalVariables()


    """ Local variables: """
    SA_Cooling = []
    SA_Cooling_list = []
    SA_Benchmarks = []
    
    """Reading Input files """
    if kwargs:
        for key in kwargs:
            if ( key == 'File_Name' ):
                FileName = kwargs[ key ]
                N_Tests = 0
            elif( key == 'No_Tests' ):
                N_Tests = kwargs[ key ]
                FileName = 'Benchmarks.in'
            else:
                sys.exit( 'Option not found' )

    else:
        sys.exit( 'Option not found' )

    icount = 0 ; ntest = 0
    SA_Testing = False


    """ Open input file containing Cooling Schedule: """

    with open( FileName, 'r' ) as f:
        
        for line in f:
            if line[ 0 ] == '$': # Variable's name
                line.rstrip()
            elif line[ 0 ] == '$': # Test Name
                line.rstrip()
                SA_Cooling_list.append( line[ 3 : len( line ) - 1 ] )
                #print 'b1:', line[ 3 : len( line ) - 1 ]
                icount += 1
            elif line[ 0 ] == '#': # Variable's name
                line.rstrip()
                SA_Cooling_list.append( line[ 2 : len( line ) - 1 ] )
                #print 'b2:', line[ 2 : len( line ) - 1 ]                
                icount += 1
            elif ( line == '\n' ): # Empty lines
                #print 'b3:', line
                line.rstrip()
            else:
                inner_list = []
                #print 'buuu:', len( SA_Cooling_list ),  ( icount - 1 ), str( SA_Cooling_list[ icount - 1 ][0:9] )
                if ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ][0:9] ) == 'Benchmark' ) ):
                    ntest += 1

                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Minimum' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]  
                    #print 'here:', inner_list[ 0 ]                  
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )


                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Debugging' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )


                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Function' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]
                    SA_Cooling.append( inner_list[ 0 ] )

                else:
                    inner_list = [ float(elt.strip()) for elt in line.split(',') ]
                    SA_Cooling.append( inner_list )


    f.close()

    for itest in range( N_Tests):
        if N_Tests > 0:
            if itest == 0:
                node = itest * 16 + itest
            else:
                node = itest * 16 + itest - 1
        else:
            node = itest * 14 + itest
            
        SA_Function.append( SA_Cooling[ node + 0 ] )
        SA_N.append( num( SA_Cooling[ node + 1 ][0] ) )
        SA_Minimum.append( SA_Cooling[ node + 2 ] )
        SA_Debugging.append( SA_Cooling[ node + 3 ] )
        SA_NS.append( num( SA_Cooling[ node + 4 ][0] ) )
        SA_NT.append( num( SA_Cooling[ node + 5 ][0] ) )
        SA_MaxEvl.append( SA_Cooling[ node + 6 ][0] )
        SA_EPS.append( SA_Cooling[ node + 7 ][0] )
        SA_RT.append(  SA_Cooling[ node + 8 ][0] )
        SA_Temp.append( SA_Cooling[ node + 9 ][0] )

        SA_LowerBounds.append( SA_Cooling[ node + 10 ] )
        SA_UpperBounds.append( SA_Cooling[ node + 11 ] )
        SA_VM.append( SA_Cooling[ node + 12 ] )
        SA_C.append( SA_Cooling[ node + 13 ] )

        if N_Tests > 0:
            SA_Xopt.append( SA_Cooling[ node + 14 ] )
            SA_Fopt.append( SA_Cooling[ node + 15 ][ 0 ] )
        else:
            SA_Xopt.append( 0. )
            SA_Fopt.append( 0. )

        """ Printing initialisation of the SA Algorithm """
        f_SAOutput.write( '\n' )
        f_SAOutput.write( '============================================================ \n' )
        f_SAOutput.write( '   Initialisation of the Simulated Annealing Algorithm: \n' )
        f_SAOutput.write( '============================================================ \n' )
        f_SAOutput.write( '\n' )
        f_SAOutput.write( 'Test Name: {a:}'.format( a = str( SA_Function[ itest ] ) ) + '\n' )
        f_SAOutput.write( 'Minimisation: {a:>10s}'.format( a = str( SA_Minimum[ itest ] ) ) + '\n' )
        f_SAOutput.write( 'Dimension-Space: {a:4d}'.format( a = SA_N[ itest ] ) + '\n' )
        f_SAOutput.write( 'Maximum Number of Function Evaluations: {a:4d}'.format( a = int( SA_MaxEvl[ itest ] ) ) + '\n' )
        f_SAOutput.write( '\n' )
        f_SAOutput.write( 'NS: {a:3d}, NT: {b:3d}'.format( a = SA_NS[ itest ], b = SA_NT[ itest ] ) + '\n' )
        f_SAOutput.write( 'EPS: {a:.4e}'.format( a = SA_EPS[ itest ] ) + '\n' )
        f_SAOutput.write( '\n' )
        f_SAOutput.write( 'Temperature: {a:.4f} \nParameter for temperature reduction (RT): {b:.4f}'.format( a = SA_Temp[ itest ], b = SA_RT[ itest ] ) + '\n' )
        f_SAOutput.write( '\n' )
        f_SAOutput.write( 'Lower Bounds: {a:}, Upper Bounds: {b:}'.format( a = SA_LowerBounds[ itest ], b = SA_UpperBounds[ itest ] ) + '\n' )
        f_SAOutput.write( 'VM: {a:}, C: {b:}'.format( a = SA_VM[ itest ], b = SA_C[ itest ] ) + '\n' )
        f_SAOutput.write( '\n' )
        f_SAOutput.write( 'Optimum Solution for Test: {a:}'.format( a = str( SA_Function[ itest ] ) + '\n' ) )
        if N_Tests > 0:
            f_SAOutput.write( '{s:10} X_Opt: {a:}'.format( s = ' ', a = SA_Xopt[ itest ] ) + '\n' )
            f_SAOutput.write( '{s:10} F_Opt: {a:}'.format( s = ' ', a = SA_Fopt[ itest ] ) + '\n' )
        f_SAOutput.write( '\n' )
        f_SAOutput.write( '============================================================ \n' )
        f_SAOutput.write( ' \n' )
        f_SAOutput.write( '============================================================ \n' )
        f_SAOutput.write( '\n' )


        

            
    return SA_Function, SA_Minimum, SA_N, SA_NS, SA_NT, SA_MaxEvl, SA_EPS, SA_RT, SA_Temp, \
        SA_LowerBounds, SA_UpperBounds, SA_VM, SA_C, SA_Debugging, SA_Xopt, SA_Fopt

    



