C	ARQUIVO FUGPURE.F

C	*********************************************************************
C	*********************************************************************
C
	SUBROUTINE FUGP(CFLPu)
C
C	*********************************************************************
C	*********************************************************************

C	ESTA SUBROTINA CALCULA O VALOR DO COEFICIENTE DE FUGACIDADE DO SOLVEN
C	TE PURO

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INCLUDE 'common.f'
	INCLUDE 'comcalc.f'

	DIMENSION COEF(4)


C	========================================
C	CALCULO DO "A" E "B" DO SOLVENTE PURO
C	========================================

	APu=Pe*AI(1)/(R*Te)**2
	BPu=Pe*BI(1)/(R*Te)

C	===========================================================
C	CALCULO DO FATOR DE COMPRESSIBILIDADE "Z" DA FASE LIQUIDA
C	===========================================================

	COEF(1) = (APu*BPu - BPu**2 - BPu**3)
	COEF(2) = (APu - 3.0D0*(BPu**2) - 2.0D0*BPu)
	COEF(3) = (1.0D0 - BPu)
	COEF(4) = BPu
      
	ICASE = 1
	METHOD = 5
	INIT = 2
	DXZERO = 1.0D-07
	FXZERO = 1.0D-07
	IDIVER = 40
	MAXIT = 1000
	
      	CALL DUPLA(ICASE,METHOD,INIT,COEF,ZVAP,ZLIQ,IDIVG)  

	write(*,*)'AI(1) : ',AI(1)
        write(*,*)'BI(1) : ',BI(1)
        write(*,*)'ZLIQ : ',ZLIQ
        write(*,*)'BPu : ', BPu


C	===================================================================
C	CALCULO DO COEFICIENTE DE FUGACIDADE DO COMP. PURO NA FASE LIQUIDA
C	===================================================================

        DLCFPu=(ZLIQ-1.0D0)-DLOG(ZLIQ-BPu)-APu/(2.0D0*DSQRT(2.0D0)*BPu)*
     &         DLOG((ZLIQ+2.414*BPu)/(ZLIQ-0.414*BPu))
        CFLPu=DEXP(DLCFPu)

        write(*,*)'CFLPu :',CFLPu

        RETURN
	END	

	




