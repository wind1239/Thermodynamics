#!/usr/bin/env python

# a function that contains the variables to calculate the greek_fi

import numpy as np
import math
import sys
import thermotools_test as ThT
import EOS_PR_test as PR
import ln_gamma_test as lng
import calculate_terms_test as terms

'''e = lngamma.kostas( 2, 2, 2, 2)
print ' the e = ', e'''

'''
# Function definition is here
def changeme( mylist ):
   "This changes a passed list into this function"
   mylist.append([1,2,3,4]);
   print "Values inside the function: ", mylist
   return

# Now you can call changeme function
mylist = [10,20,30];
changeme( mylist );
print "Values outside the function: ", mylist
'''

MFrac = 1

partial1 = 1 / 1 - terms.D( MFrac )
print ' partial 1 = ', partial1




MFrac = [ 0 , 0 ]


def calc_B( MFrac ):
    b = terms.B( MFrac )
    fi = b
    return fi

phi = calc_B( MFrac )

print ' fi = ', phi 




    
