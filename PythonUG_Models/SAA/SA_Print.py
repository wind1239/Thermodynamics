
#!/usr/bin/env python

import sys
import SA_IO as IO


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

    if kwargs.get( 'Initialisation', None ) == 'yes':
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '   Initialisation of the Simulated Annealing Algorithm:      \n' )
        IO.f_SAOutput.write( '      Test Name: {a:}'.format( a = str( IO.SA_Function ) ) +      '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '\n' )
        
    else:
        IO.f_SAOutput.write( '\n' )
        IO.f_SAOutput.write( '============================================================ \n' )
        IO.f_SAOutput.write( '        Simulated Annealing algorithm diagnostics:           \n' )
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
    
    if kwargs.get( 'Initialisation', None ) == 'yes':
        IO.f_SAOutput.write( 'Inital Solution for Test: {a:}'.format( a = str( IO.SA_X ) + '\n' ) )
        



