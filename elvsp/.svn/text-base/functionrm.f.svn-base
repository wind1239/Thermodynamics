
C     ARQUIVO FUNCTIONRM.F

C     ******************************************************************
C     ******************************************************************

      SUBROUTINE FCN(N,TETA,H,ATIVL)

C     ******************************************************************
C     ******************************************************************

C     ==================================================================
C
C       	            CALCULO DA FUNCAO H      
C
C     ==================================================================
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      DIMENSION TETA(ND),GAMA2(ND),FMS(ND),ATIVL(ND)

      
      IF(IJI.EQ.0)THEN
      	IF(ALFAL.GT.ALFAV)THEN
      		CALL MIVFUNRM(N,TETA,IER6V,GAMA2)
      	ELSE
      		CALL MILFUNRM(N,TETA,IER6L,GAMA2,ATIVL)
      	ENDIF     
      ELSE
      	IF(ALFAL.GT.ALFAV)THEN
      		CALL MIVFUNRM(N,TETA,IER6V,GAMA2)
      		CALL AMILFUNRM(N,TETA,IER6AL,GAMA1,ATIVL)
      	ELSE
      		CALL MILFUNRM(N,TETA,IER6L,GAMA2,ATIVL)
      		CALL AMIVFUNRM(N,TETA,IER6AV,GAMA1)
      	ENDIF
      ENDIF
      	
C     ==================================================================
C
C     			    AVALIACAO DA FUNCAO H      	
C
C     ==================================================================
      
      H2C = 0.0D0 
      H2C = GAMA1(N) - GAMA2(N)
      
      SS1 = 0.0D0
      DO 20 I = 1,N-1
        FMS(I) = TETA(I)
        SS1 = FMS(I) + SS1
 20   CONTINUE
      FMS(N) = 1.0D0 - SS1

      SM1 = 0.0D0
      SM2 = 0.0D0
      SM3 = 0.0D0
      SM4 = 0.0D0

      DO 50 I = 1,N-1
        SM1 = FMS(I)*((GAMA1(I)-GAMA2(I))-H2C)+SM1
 50   CONTINUE

      SM2 = TETA(N)*SM1

      DO 100 I = 1,N
        SM3 = SM3 + Ze(I)*GAMA2(I)
 100  CONTINUE

      SM4 = TETA(N)*H2C

      IF(IJI.EQ.0)THEN

        IF((IER6V.EQ.0).AND.(IER6L.EQ.0))THEN
             H = 0.0D0
        ELSE
             H = SM2 + SM4 + SM3
        ENDIF
   
      ELSE

        IF((IER6V.EQ.0).OR.(IER6AL.EQ.0))THEN
          IVER1 = 0
        ELSE
          IVER1 = 1
        ENDIF

        IF((IER6L.EQ.0).OR.(IER6AV.EQ.0))THEN
          IVER2 = 0
        ELSE
          IVER2 = 1
        ENDIF
 
        IF((IVER1.EQ.0).AND.(IVER2.EQ.0))THEN
          H = 0.0D0
        ELSE
          H = SM2 + SM4 + SM3
        ENDIF

      ENDIF     
            
      ITER = ITER + 1
C     WRITE(*,*)'FUNCT : ',H
C     WRITE(*,*)'ATIVL(1) : ',ATIVL(1)
      
      RETURN
      END
