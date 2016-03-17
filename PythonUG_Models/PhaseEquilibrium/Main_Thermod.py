
#!/usr/bin/env python

import matplotlib.pyplot as bplot
import numpy as np
import math
import ThermoTools as ThT
import GibbsFunction as GibbsF
import PhaseStability as Michaelsen
import NormalisedConstraint as NC
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
    ThT.MFrac = 0. ; ThT.PhaseFrac = 0. ; Inc_Phase = 0.25 ; Inc_Comp = 0.1
    NearlyOne = 1. - ThT.Residual

    Niter_Phase = int( 1. / Inc_Phase ) + 1
    Niter_Comp = int( 1. / Inc_Comp ) + 1
    Molar_Gibbs_Free = [ 0. for i in range( Niter_Phase * Niter_Comp ) ]
    Composition  = [ 0. for i in range( Niter_Phase * Niter_Comp ) ] 

    for iter_phase in range( Niter_Phase ):

        sumphase = 0.; PhaFrac = [ 0. for i in range( ThT.NPhase ) ]
        for iphase in range( ThT.NPhase - 1 ):
            PhaFrac[ iphase ] = min( NearlyOne, max( ThT.Residual, float( iter_phase ) * Inc_Phase ) )
            sumphase = sumphase + PhaFrac[ iphase ] 
        PhaFrac[ ThT.NPhase - 1 ] = 1. - sumphase
        ThT.PhaseFrac = PhaFrac


        for iter_comp in range( Niter_Comp ):

            
            sum2 = 0. ; MolFrac = [ 0. for i in range( ThT.NComp * ThT.NPhase ) ]
            for iphase in range( ThT.NPhase - 1 ):
                for icomp in range( ThT.NComp - 1 ):
                    node = iphase * ThT.NComp + icomp
                    MolFrac[ node ] = min( NearlyOne, max( ThT.Residual, float( iter_comp ) * Inc_Comp ) )
                    sum2 = sum2 + MolFrac[ node ]
                node2 = iphase * ThT.NComp + ThT.NComp -1
                MolFrac[ node2 ] = PhaFrac[ iphase ]
                #MolFrac[ node2 ] = 1. - sum2

            MolFrac = NC.CalcOtherPhase( MolFrac, ThT.NPhase - 1 )
            ThT.MFrac = MolFrac

            for iphase in range( ThT.NPhase ):
                sumcomp = 0.
                for icomp in range( ThT. NComp ):
                    node = iphase * ThT.NComp + icomp
                    if icomp == ThT.NComp - 1:
                        ThT.MFrac[ node ] = 1. - sumcomp
                            
                    else:
                        sumcomp = sumcomp + MolFrac[ node ]

            #print 'MFrac:', ThT.MFrac
            #print 'PhaseFrac:', ThT.PhaseFrac
   

            '''
               ===============================================================
                  MICHAELSEN'S STABILITY TEST: Decision of the Phases 
               ===============================================================
                                                                              '''
            ( Comp, Comp_Phase, GZero ) = Michaelsen.Phase_Stability( Temp, Press )
            Michaelsen.CheckingPhases( Comp_Phase, GZero )

            #print 'GZero:', GZero, Comp_Phase


            InitialAssessment = False
            Molar_Gibbs_Free[ iter ] = GibbsF.GibbsObjectiveFunction( InitialAssessment, Temp, Press, Comp_Phase )
            Composition[ iter ] = [ MolFrac[ 0 ], MolFrac[ 2 ] ]

            pickle.dump( Comp_Phase, OutFile )


            print 'Comp_Phase:', Comp_Phase
            print 'Molar Gibbs Free Energy:', Molar_Gibbs_Free

            print '  '

        OutFile.close()

        f = open("output")
        data = pickle.load( f )
        print data


        print ' ++++++++++++ '
        for iter in range( Niter + 1 ):
            print Composition[ iter ], Molar_Gibbs_Free[ iter ] """



