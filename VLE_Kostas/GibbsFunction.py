
#!/usr/bin/env python

import numpy as np
import math
import sys
import ThermoTools as ThT


def Calc_Gibbs( Temp, Press, MFrac, PhaseFrac ):
    GibbsEnergy = sys.float_info.max

    ''' the purpose of this function is to take the eq. 1.20, Jeff's notes and try to connect with the rest of the functions.
        This is the Statement of the VLE problem, where T, P, zi = feed fraction of component i are given! The goal is to calculate 
        the w1(L) or the mass fraction of component i in Liquid face that L minimizes! '''

    ''' regarding the 1.20, what i have to do is to retrieve the values 

        zi = given from input

        mu (greek m = chemical potential) = taken from the 1.18 (calling 1.18), with known P, R, T! 
                                            while the fugacity coef (calling EOS to calculate it), - or greek f - 
                                            is taken from the 2.23 by calling it 
                                            > i know the Big_B, am, bm, R, T  
                                            > the z - compressibility factor in 2.23 will come out from the cubic polyonmial, 
                                              roots of the cubic polynomial ( check the Big_A and Big_B based on the EOS 
                                              we are using I have to change the polynomial ?) '''
   
 
    return GibbsEnergy
    



    
