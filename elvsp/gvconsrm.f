
C     ARQUIVO GVCONSRM.F
C
C     ******************************************************************
C     ******************************************************************
C
C     OPCAO DE ESCOLHA : FASE VAPOR (XV(1),XV(2),...,XV(N-1),V)
C                        CALCULO DE GV 
C
C     ******************************************************************
C     ******************************************************************

      SUBROUTINE MIVCONSRM(N,IER6,GV)
      
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      DIMENSION COEF(4)
      DIMENSION DPAM(ND),DPBM(ND)
      DIMENSION GV(ND),COMP(ND)
      
      	
C     ******************************************************************
C     ******************************************************************
C
C     CALCULO DOS COEFICIENTES DE FUGACIDADE RELATIVOS AOS COMPONENTES
C     PRESENTES NA FASE VAPOR
C
C     ******************************************************************
C     ******************************************************************

      CALL CONVW(N,Ze,COMP)
      
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


C     ==================================================================
C 
C     CALCULO DO LOGARITMO DO COEFICIENTE DE FUGACIDADE (DLCFL(I)), DA
C     FUGACIDADE (CFUGL(I)) E DO POTENCIAL QUIMICO (GL(I))
C
C     ==================================================================
   
      NAT = 0

      CALL POTQUIM(N,NAT,COMP,ZVAP,A,B,BS,DPBM,AS,DPAM,IER6,GV,ATIVL)

 
      RETURN
      END 

     
