
C     ARQUIVO AGLRM.F


C     ******************************************************************
C     ******************************************************************

      SUBROUTINE AMILFUNRM(N,TETA,IER6,GL,ATIVL)
      
C     ******************************************************************
C     ******************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION DPAM(ND),DPBM(ND),COEF(4)
      DIMENSION GL(ND),ATIVL(ND)
      DIMENSION XL(ND),TETA(ND) ,COMP(ND)
      
      
      SXL = 0.0D0
      
      DO 20 I = 1,N-1
      	XL(I) = TETA(I)
        SXL = SXL + XL(I)
 20   CONTINUE
 
      XL(N) = 1.0D0 - SXL

      CALL CONVW(N,XL,COMP)
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
      INIT = 2
      DXZERO = 1.0D-07
      FXZERO = 1.0D-07
      IDIVER = 40
      MAXIT = 1000
	
      CALL DUPLA(ICASE,METHOD,INIT,COEF,ZVAP,ZLIQ,IDIVG)   
      

C     WRITE(*,*)'ZPQL :',ZPQ   


C     ==================================================================
C 
C     CALCULO DO LOGARITMO DO COEFICIENTE DE FUGACIDADE (DLCFL(I)), DA
C     FUGACIDADE (CFUGL(I)) E DO POTENCIAL QUIMICA (GL(I))
C
C     ==================================================================

      NAT = 1

      CALL POTQUIM(N,NAT,COMP,ZLIQ,A,B,BS,DPBM,AS,DPAM,IER6A,GL,ATIVL)
C     WRITE(77,*)'IER6AGL :',IER6
 
      RETURN
      END      
