
#!/usr/bin/env python

import sys


###
###
###
def HelpInput():
    print ' '
    print 'Missing argument, command line should be:'
    print ' '
    print "python Optimiser.py <Method = 'SA'> <Task = 'Benchmarks'> "
    print "              OR  "
    print "python Optimiser.py <Method = SA>  <Task = 'Problem'> <File Name >"
    print ' '
    print "e.g., python Optimiser.py SA Benchmarks"
    print "      python Optimiser.py SA Problem    VLE_MethanePentane.in "
    print ' '
    print " Info for the benchmark test-cases must be contained in the file"
    print " Benchmarks.in"
    print ' '
