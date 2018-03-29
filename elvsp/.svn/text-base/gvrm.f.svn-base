
C     ARQUIVO GVRM.F


C     ******************************************************************
C     ******************************************************************

      SUBROUTINE MIVFUNRM(N,TETA,IER6,GV)
      
C     ******************************************************************
C     ******************************************************************


C     ==================================================================
C
C     OPCAO DE ESCOLHA : FASE LIQUIDA GV(XL(1),XL(2),...,XL(N-1),L)
C     CALCULO DA FUNCAO GV (POTENCIAL QUIMICO DA FASE VAPOR)
C
C     ==================================================================

      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION COEF(4)
      DIMENSION DPAM(ND),DPBM(ND)
      DIMENSION GV(ND),ATIVL(ND)
      DIMENSION XL(ND),XV(ND),TETA(ND),COMP(ND)


C     ==================================================================
C     
C                CALCULO DE XV(I) A PARTIR DE XL(I)
C
C     ==================================================================

     
      SSOMA = 0.0D0
      
      DO 50 I =1,N-1
      	XL(I) = TETA (I)
        XTZ = (Ze(I) - TETA(I)*TETA(N))/(1.0D0 - TETA(N))
        SSOMA = SSOMA + XTZ
 50   CONTINUE
      
      ELE = TETA(N)
      V = 1.0D0 - ELE

      SOMAXL = 0.0D0   
      DO 60 I = 1,N-1
      	SOMAXL = XL(I) + SOMAXL
 60   CONTINUE
 
      XL(N) = 1.0D0 - SOMAXL
      
      XSOMAV = 0.0D0
      
      DO 70 I =1,N-1
      	XV(I) = (Ze(I) - ELE*XL(I))/V
      	XSOMAV = XV(I) + XSOMAV
 70   CONTINUE
  
      XV(N) = 1.0D0 - XSOMAV

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
      

C     ==================================================================
C 
C     CALCULO DO LOGARITMO DO COEFICIENTE DE FUGACIDADE (DLCFV(I)), DA
C     FUGACIDADE (CFUGV(I)) E POTENCIAL QUIMICO (GV(I))
C
C     ==================================================================

      NAT = 0

      CALL POTQUIM(N,NAT,COMP,ZVAP,A,B,BS,DPBM,AS,DPAM,IER6,GV,ATIVL)
   
 
      RETURN
      END      
