

C     ARQUIVO AGVRM.F


C     ******************************************************************
C     ******************************************************************

      SUBROUTINE AMIVFUNRM(N,TETA,IER6A,GV)
      
C     ******************************************************************
C     ******************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION COEF(4),ATIVL(ND)
      DIMENSION DPAM(ND),DPBM(ND)
      DIMENSION XV(ND),GV(ND),TETA(ND),COMP(ND)
      
      
      SXV = 0.0D0
      
      DO 20 I = 1,N-1
      	XV(I) = TETA(I)
        SXV = SXV + XV(I)
 20   CONTINUE

      XV(N) = 1.0D0 - SXV

      CALL CONVW(N,XV,COMP)
      CALL MIXRULE(N,COMP,A,B,AS,BS,DPAM,DPBM)
           
C     ==================================================================
C
C     CALCULO DOS FATORES DE COMPRESSIBILIDADE PELA CHAMADA DA SUBROTINA
C     'DUPLA'.
C
C     ==================================================================      
      
      COEF(1) = (A*B - B**2 - B**3)
      COEF(2) = (A - 3.0D0*(B**2) - 2.0D0*B)
      COEF(3) = (1.0D0 - B)
      COEF(4) = B
      
      ICASE = 1
      METHOD = 5
      INIT = 1
      DXZERO = 1.0D-07
      FXZERO = 1.0D-07
      IDIVER = 40
      MAXIT = 1000
	
      CALL DUPLA(ICASE,METHOD,INIT,COEF,ZVAP,ZLIQ,IDIVG)   
      
C     WRITE(*,*)'ZPQV :',ZPQ 


C     ==================================================================
C 
C     CALCULO DO LOGARITMO DO COEFICIENTE DE FUGACIDADE (DLCFV(I)), DA
C     FUGACIDADE (CFUGV(I)) E POTENCIAL QUIMICO (GV(I))
C
C     ==================================================================

      NAT = 0

      CALL POTQUIM(N,NAT,COMP,ZVAP,A,B,BS,DPBM,AS,DPAM,IER6A,GV,ATIVL)
C     WRITE(77,*)'IER6AGV :',IER6
 
      RETURN
      END 
   
