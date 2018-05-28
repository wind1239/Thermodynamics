
#!/usr/bin/env python   
import sys
import SystemPaths as SyP
import RandomGenerator as RanGen
import SAA_Tools as SaT
import pdb


###
### Summation of a list of reals
###
def ListSum( XList ):
    if ( len( XList ) == 1 ):
        return XList[ 0 ]
    else:
        return XList[ 0 ] + ListSum( XList[ 1 : ] )


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
### Imposing constraints into field variables
###
def Envelope_Constraints( Method, Task, X, **kwargs ):
    """ This function ensures that variable 'X' is bounded """
    SyP.EnvirVar( Task, Method, Thermodynamics = 'Thermodynamics' )
    import ThermoTools as ThT

    rand = []
    TryAgain = True
    IsNormalised = True

    if kwargs:
        for key in kwargs:
            if ( key == 'LBounds' ):
                LowerBounds = kwargs[ key ]
            elif ( key == 'UBounds' ):
                UpperBounds = kwargs[ key ]
            elif ( key == 'NDim' ):
                n = kwargs[ key ]
            elif ( key == 'TryC' ):
                Try = kwargs[ key ]
            elif ( key == 'IsNormalised' ):
                IsNormalised = kwargs[ key ]
            elif ( key == 'Z_Feed' ):
                Z_Feed = kwargs[ key ]

    else:
         LowerBounds = SaT.LowerBounds
         UpperBounds = SaT.UpperBounds
         n = SaT.Ndim
    
    evaluations = 1
    
    if ( IsNormalised ): # Mole/Mass/Volume Fraction (thermodynamic problem)
        dim = n - 1
    else:
        dim = n

    SumOneOtherPhase = True
        
    while TryAgain:

        #pdb.set_trace()
        for i in range( n ):
            if ( ( X[ i ] < LowerBounds[ i ] ) or ( X[ i ] > UpperBounds[ i ] ) or ( not SumOneOtherPhase ) or ( not Try )):
                rand = RanGen.RandomNumberGenerator( n )
                X[ i ] = LowerBounds[ i ] + ( UpperBounds[ i ] - LowerBounds[ i ] ) * \
                    rand[ i ]
                X[ i ] = min( max( LowerBounds[ i ], X[ i ] ), UpperBounds[ i ] )
                Try = True ; print 'farofa666', not IsNormalised

        if ( not IsNormalised ): 
            if kwargs:
                for key in kwargs:
                    if ( key == 'TryC' ):
                        return X, Try
                    else:
                        return X
            else:
                return X

        else: # Thermod problem

            if  ListSum( X[ 0 : dim ] ) < 1.:
                SumOneOtherPhase = CheckOtherPhase( Task, Method, X, UpperBounds, LowerBounds )
                if SumOneOtherPhase:
                    if Try:
                        return X, Try
                    else:
                        return X
            else:
                Try = False
                    #if kwargs:
                    #    for key in kwargs:
                    #        if ( key == 'TryC' ):
                    #            sys.exit('farofa1') ; return X, Try
                    #        else:
                    #            return X
                    #else:
                    #    return X


###
### Checking the other phase
###
def CheckOtherPhase( Task, Method, X, UB, LB, **kwargs ):
    """ This function calculates the composition of the other phase. The input is X[0:NComp*NPhase-1],
        where NComp-1 mole/mass fractions are stored for all phases (NPhase). The address
                  iphase * (NComp - 1 ) , with iphase = 0, NPhase-1,
        contains the mass/mole fraction of phase PiPhase. Z[0:N-1] is the feed composition. UB and LB
        are the lower and upper bound arrays. In 2 phases (vapour and liquid)
            MFrac[0 : N-1 ]: mol fraction of components in the liquid phase;
            MFrac[N : 2*N - 1]: mol fraction of components in the vapour phase.
            However, for generality the function is designed for an arbitrary number of
            phases. """
    SyP.EnvirVar( Task, Method, Thermodynamics = 'Thermodynamics' )
    import ThermoTools as ThT

    
    NC = ThT.NComp ; NP = max( 1, ThT.NPhase )
    MFrac = [ 0. for i in range( NC*NP ) ] # Generating null arrays
    PhFrac = [ 0. for i in range( NP ) ] #
    TestOtherPhase = True

    for iphase in range(NP-1):
        for icomp in range(NC):
            node_comp = iphase * NC + icomp ; node_comp0 = iphase * NC
            if icomp < ( NC - 1 ):
                MFrac[ node_comp ] = X[ node_comp ]
            else:
                PhFrac[ iphase ] = X[ node_comp ]
                MFrac[ node_comp ] = 1. - ListSum( MFrac[ node_comp0 : node_comp ] )

    PhFrac[ NP - 1 ] = 1. - ListSum( PhFrac[ 0 : NP - 1 ] )

    iphase = NP - 1 # Checking the last phase
    for icomp in range(NC):
        node_comp = iphase * NC + icomp ; node_comp0 = iphase * NC ; sum1 = 0. ; sum2 = 0.
        for jphase in range(NP):
            if jphase != iphase:
                node_comp2 = jphase * NC + icomp
                sum1 = sum1 + MFrac[ node_comp2 ] * PhFrac[ jphase ]
                sum2 = sum2 + PhFrac[ jphase ]
        MFrac[ node_comp ] = ( ThT.Z_Feed[ icomp ] - sum1 ) / ( 1. - max( sum2, ThT.Residual ) )


    if kwargs: # extra diagnostics ... print the components mol fraction and phase mol fraction
        for key in kwargs:
            if ( key == 'Diagnostics' ):
                Diag = kwargs[ key ]
                if Diag:
                    IO.f_SAOutput.write( '\n' )
                    IO.f_SAOutput.write( 'Composition (liq and vap): {a:}'.format( a = MFrac ) + '\n' )
                    IO.f_SAOutput.write( 'Liquid Phase Molar Fraction: {a:}'.format( a = PhFrac[0] ) + '\n' )
                    
    for iphase in range(NP): # Checking bounds at all phases
        for icomp in range(NC):
            node_comp = iphase * NC + icomp
            if MFrac[ node_comp ] < LB[ icomp ] or MFrac[ node_comp ] > UB[ icomp ] or \
               PhFrac[ iphase ] < LB[ icomp ] or PhFrac[ iphase ] > UB[ icomp ]:
                TestOtherPhase = False
                return TestOtherPhase


        nodecomp0 = iphase * NC ; nodecomp1 = iphase * NC  + (NC - 1 )

        if abs( ListSum( MFrac[ nodecomp0 : nodecomp1 + 1 ] ) - 1. ) >= ThT.Residual:
                TestOtherPhase = False
                return TestOtherPhase


    return TestOtherPhase
           
