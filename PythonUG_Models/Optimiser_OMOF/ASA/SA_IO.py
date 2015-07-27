
#!/usr/bin/env python

import sys

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
def ReadInCoolingSchedule( **kwargs ):
    
    """Reading Input files """
    if kwargs:
        for key in kwargs:
            if ( key == 'File_Name' ):
                FileName = kwargs[ key ]
                ntest = 0

    else:
        FileName = 'Benchmarks.in'

        
    SA_Cooling = []
    SA_Cooling_list = []
    
    icount = 0 ; ntest = 0
    SA_Testing = False


    """ Open input file containing Cooling Schedule: """

    with open( FileName, 'r' ):
        
        for line in f:
            if line[ 0 : 1 ] == '$$': # Test Name
                line.rstrip()
                SA_Cooling_list.append( line[ 3 : len( line ) - 1 ] )
                icount += 1
            elif line[ 0 ] == '#': # Variable's name
                line.rstrip()
                SA_Cooling_list.append( line[ 2 : len( line ) - 1 ] )
                icount += 1
            elif ( line == '\n' ): # Empty lines
                line.rstrip()
            else:
                inner_list = []
                if ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ][0:9] ) == 'Benchmark' ) ):
                    ntest += 1

                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Minimum' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]                    
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )
                    
                    
                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Debugging' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )
                    
                    
                elif ( ( len( SA_Cooling_list ) >= ( icount - 1 ) ) and ( str( SA_Cooling_list[ icount - 1 ] ) == 'Testing' ) ):
                    inner_list = [ elt.strip() for elt in line.split(',') ]
                    SA_Cooling.append( to_bool( inner_list[ 0 ] ) )
                    
                else:
                    inner_list = [ float(elt.strip()) for elt in line.split(',') ]
                    SA_Cooling.append( inner_list )

    f.close()
        

    



