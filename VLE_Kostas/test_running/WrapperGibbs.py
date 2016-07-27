#!/usr/bin/env python

import sys
import gibbsfunction_test as GBT



def Wrapper( Ndim, XSolution ):

    MolarGibbs = GBT.GibbsCalculation( XSolution )

    return MolarGibbs
