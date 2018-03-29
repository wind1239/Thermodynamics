C     ARQUIVO CONTA.F

      SUBROUTINE CALC(N,IEOS)
	
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'


      DIMENSION DK0(ND),DK(ND)


C	==============================================================
C
C	ESTA ROTINA CALCULA OS PARAMETROS DE ATRACAO E REPULSAO PELA
C	REGRA DE COMBINACAO DE PENG-ROBINSON(IEOS=0) OU DE PENG-ROBIN
C	SON-STRYJEK-VERA(IEOS=1)
C	
C	==============================================================

        IF(IEOS.EQ.0)THEN
          BI(1) = 0.0788D0*R*Tc(1)/Pc(1)
	  DK(1) = 0.37464D0+1.54226D0*W(1)-0.26992D0*(W(1)**2)
	  ALFA(1) = 1.0D0+DK(1)*DSQRT(1.0D0-(Tr(1)**2))
	  AI(1) = ALFA(1)*(0.45724D0*((R*Tc(1))**2)/PC(1)) 
        ELSE    
	  BI(1) = (0.077796D0*R*Tc(1))/Pc(1)
 	  DK0(1) = 0.378893D0+1.4897153D0*W(1)-0.17131848D0*
     +          (W(1)**2)+ 0.0196554*(W(1)**3)
          DK(1) = DK0(1) + DK1(1)*(1.0D0 + DSQRT(Tr(1)))*(0.7D0 - 
     +          Tr(1))
     	  ALFA(1)=(1.0D0+DK(1)*DSQRT(1.0D0-Tr(1)))**2
     	  AI(1) = 0.457235D0*((R*Tc(1))**2)*ALFA(1)/Pc(1)
        ENDIF

 
        RETURN
        END
     
