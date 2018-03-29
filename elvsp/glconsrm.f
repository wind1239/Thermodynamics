
C     ARQUIVO GLCONSRM.F      

C     ******************************************************************
C     ******************************************************************
C     
C        OPCAO DE ESCOLHA : FASE LIQUIDA (XL(1),XL(2),..,XL(N-1),L)
C                           CALCULO DE GL
C	 
C     ******************************************************************
C     ******************************************************************
      
      
      SUBROUTINE MILCONSRM(N,IER6,GL)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      DIMENSION COEF(4)
      DIMENSION DPAM(ND),DPBM(ND)
      DIMENSION GL(ND),COMP(ND)

      CALL CONVW(N,Ze,COMP)      

      CALL MIXRULE(N,COMP,A,B,AS,BS,DPAM,DPBM)
		
C     ******************************************************************
C     ******************************************************************
C
C     CALCULO DOS COEFICIENTES DE FUGACIDADE RELATIVOS AOS COMPONENTES
C     PRESENTES NA FASE LIQUIDA
C
C     ******************************************************************
C     ******************************************************************

      
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
      
C     ==================================================================
C 
C     CALCULO DO LOGARITMO DO COEFICIENTE DE FUGACIDADE (DLCFL(I)), DA
C     FUGACIDADE (CFUGL(I)) E DO POTENCIAL QUIMICO (GL(I))
C
C     ==================================================================

      NAT = 0

      CALL POTQUIM(N,NAT,COMP,ZLIQ,A,B,BS,DPBM,AS,DPAM,IER6,GL,ATIVL)

      RETURN
      END  
