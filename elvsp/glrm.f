
C     ARQUIVO GLRM.F


C     ******************************************************************
C     ******************************************************************

      SUBROUTINE MILFUNRM(N,TETA,IER6,GL,ATIVL)
      
C     ******************************************************************
C     ******************************************************************


C     ==================================================================
C
C     OPCAO DE ESCOLHA : FASE VAPOR GL(XV(1),XV(2),...,XV(N-1),V)
C     CALCULO DA FUNCAO GL (POTENCIAL QUIMICO DA FASE LIQUIDA)
C
C     ==================================================================


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION COEF(4)
      DIMENSION DPAM(ND),DPBM(ND)
      DIMENSION GL(ND),ATIVL(ND)
      DIMENSION XL(ND),XV(ND),TETA(ND),COMP(ND)



C     ==================================================================
C     
C                CALCULO DE XL(I) A PARTIR DE XV(I)
C
C     ==================================================================

      DO 40 I = 1,N-1
      	XV(I) = TETA (I)
 40   CONTINUE
 
      V = TETA(N)
      ELE = 1.0D0 - V
      
      SUMXV = 0.0D0
      SUMXL = 0.0D0
      
      DO 50 I = 1,N-1
      	XL(I) = (Ze(I) - V*XV(I))/ELE
      	SUMXV = XV(I) + SUMXV
      	SUMXL = XL(I) + SUMXL   	
 50   CONTINUE
 
      XV(N) = 1.0D0 - SUMXV
      XL(N) = 1.0D0 - SUMXL

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

      CALL POTQUIM(N,NAT,COMP,ZLIQ,A,B,BS,DPBM,AS,DPAM,IER6,GL,ATIVL)
C     WRITE(77,*)'IER6GL :',IER6
 
      RETURN
      END 
