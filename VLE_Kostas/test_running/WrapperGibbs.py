#!/usr/bin/env python

import sys
import gibbsfunction_test as GBT



def Wrapper( Ndim, XSolution ):

    print 'I am here'

    MolarGibbs = GBT.GibbsCalculation( XSolution )

    return MolarGibbs
