
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math
import ThermoTools as ThT
import GibbsFunction as GibbsF
import PhaseStability as Michaelsen
import pickle
#import csv # Using csv (comma separated values) module


''' This is the main program that aims to calculate the vapour-liquid equilibrium
       composition of a mixture containing "NComp" components. The thermodynamic
       formulation can be found in the documentation under the ../doc directory.
'''


# Reading Input data from external file:
ThT.ReadSet_Global_Variables()

OutFile = open('output', 'w')

''' ====================================================================
       Printing the data from the input file 
    ===================================================================='''

print 'Temp of the mixture (initial, final, increment):', ThT.T_System
print 'Pres of the mixture (initial, final, increment):', ThT.P_System
#
print 'Species: ', ThT.Species
print 'Feed Composition: ', ThT.Z_Feed
print 'Critical Temperature:', ThT.T_Crit
print 'Critical Pressure:', ThT.P_Crit
print 'Molar Mass:', ThT.MolarMass
#
print 'Binary Parameter:', ThT.BinaryParameter
#
print '******  Initial guess composition   *******'
for i in range( ThT.NPhase ):
    print 'Phase:', i, ':', ThT.MFrac[ i * ThT.NComp : i * ThT.NComp + ThT.NComp ]

print 'Composition of the phases:', ThT.PhaseFrac

''' ====================================================================
       Printing the data from the input file 
    ===================================================================='''

''' Initialising Temperature and Pressure '''
Temp = ThT.T_System[ 0 ]
Press = ThT.P_System[ 0 ] 



if ThT.NPhase > 2:
    print 'This function was hacked to work only on a 2-phases system, it thus needs to be generalised.'
    sys.exit()

else:
    # Loop for automatically generating composition
    ThT.MFrac = 0. ; ThT.PhaseFrac = 0. ; inc = 0.1 ; zero = 0. ; Niter = 10
    Molar_Gibbs_Free = [ 0. for i in range( Niter + 1 ) ]
    Composition  = [ 0. for i in range( Niter + 1) ] 

    for iter in range( Niter + 1 ):
        sum1 = 0.; PhaFrac = [ 0.05 for i in range( ThT.NPhase ) ]
        for iphase in range( ThT.NPhase - 1 ):
            PhaFrac[ iphase ] = PhaFrac[ iphase ] + float(iter) * 1./float(Niter)
            sum1 = sum1 + PhaFrac[ iphase ]
        PhaFrac[ ThT.NPhase - 1 ] = 1. - sum1
        ThT.PhaseFrac = PhaFrac

        sum2 = 0. ; MolFrac = [ 0.05 for i in range( ThT.NComp * ThT.NPhase ) ]
        for iphase in range( ThT.NPhase - 1 ):
            for icomp in range( ThT.NComp - 1 ):
                node = iphase * ThT.NComp + icomp
                MolFrac[ node ] = MolFrac[ node ] + float(iter) * 1./float(Niter)
                sum2 = sum2 + MolFrac[ node ]
            node2 = iphase * ThT.NComp + ThT.NComp -1
            MolFrac[ node2 ] = 1. - sum2
        MolFrac = GibbsF.CalcOtherPhase( MolFrac, PhaFrac[0] )
        ThT.MFrac = MolFrac

        '''
           ===============================================================
              MICHAELSEN'S STABILITY TEST: Decision of the Phases 
           ===============================================================
                                                                          '''
        ( Comp, Comp_Phase, GZero ) = Michaelsen.Phase_Stability( Temp, Press )
        Michaelsen.CheckingPhases( Comp_Phase, GZero )

        print 'GZero:', GZero, Comp_Phase


        InitialAssessment = False
        Molar_Gibbs_Free[ iter ] = GibbsF.GibbsObjectiveFunction( InitialAssessment, Temp, Press, Comp_Phase )
        Composition[ iter ] = MolFrac[ 0 ]

        pickle.dump( Comp_Phase, OutFile )


        #print 'Comp_Phase:', Comp_Phase
        #print 'Molar Gibbs Free Energy:', Molar_Gibbs_Free

        print '  '

    OutFile.close()

    f = open("output")
    data = pickle.load( f )
    print data


    print ' ++++++++++++ '
    for iter in range( Niter + 1 ):
        print Composition[ iter ], Molar_Gibbs_Free[ iter ]



