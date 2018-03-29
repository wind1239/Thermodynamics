
C     ARQUIVO MIXR.F

C     **************************************************************
C     **************************************************************
C
      SUBROUTINE MIXRULE(N,COMP,A,B,AS,BS,DPAM,DPBM)
C
C     **************************************************************
C     **************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'

      DIMENSION COMP(ND),DLCAT(ND),DPDN(ND),DPQN(ND),DPQN1(ND)
      DIMENSION DPAM(ND),DPBM(ND),FRV(ND)




C     'ZERANDO' AS VARIAVEIS 

      DO 150 I = 1,N
        	FRV(I) = 0.0D0
                DLCAT(I) = 0.0D0
      		DPDN(I) = 0.0D0
      		DPQN(I) = 0.0D0
      		DPQN1(I) = 0.0D0
      		DPAM(I) = 0.0D0
      		DPBM(I) = 0.0D0
 150  CONTINUE
 
      AS = 0.0D0
      BS = 0.0D0
      CS = 0.0D0
      QS = 0.0D0
      DS = 0.0D0
      DS1 = 0.0D0
      A = 0.0D0
      B = 0.0D0
      DENM = 0.0D0
      

C     *******************
C     *******************      
      
      IF(IRM.EQ.1)THEN

C
C     *******************
C     *******************

C     (SE IRM=1, EH USADO A REGRA DE MISTURAS DE WONG-SANDLER,
C      SE IRM=0, EH USADO ENTAO A REGRA DE MISTURAS CLASSICA)


C     ##################################################################
C
C     CALCULO DE 'A' E 'B', A PARTIR DAS REGRAS DE MISTURA PROPOSTAS POR
C     'WONG & SANDLER' (1992), QUE SERAO USADOS NO CALCULO DOS FATORES 
C     DE COMPRESSIBILIDADE.
C
C     ##################################################################


C     ==================================================================
C
C     CALCULO DA FUNCAO TERMODINAMICA ENERGIA LIVRE DE HELMHOLTZ DE EX-
C     CESSO (AEX)
C
C     ==================================================================

C     CALCULO DA FRACAO EM VOLUME (FRV(I))

      DENM = COMP(1) + SEGMT*COMP(2)
      FRV(1) = COMP(1)/DENM
      FRV(2) = SEGMT*COMP(2)/DENM

C     CALCULO DA AEX

      COLCH = COMP(1)*DLOG(FRV(1)/COMP(1))+COMP(2)*DLOG(FRV(2)/COMP(2))
      AEX=R*Te*(COLCH + PFR*DENM*FRV(1)*FRV(2))


C     ==================================================================
C
C     			CALCULO DE 'AM (AS)' E 'BM (BS)'
C
C     ==================================================================


C     CALCULO DE QS

      DO 300 I = 1,N
        DO 250 J = 1,N
          QS = COMP(I)*COMP(J)*0.5D0*((BI(I)+BI(J))-
     +         (AI(I)+AI(J))/(R*Te))*(1.0D0-DELT(I,J))+QS
 250    CONTINUE
 300  CONTINUE

     
C     CALCULO DE DS

      DO 450 I = 1,N
        DS1 = COMP(I)*AI(I)/(R*Te*BI(I)) + DS1
 450  CONTINUE

      CS = (1.0D0/DSQRT(2.0D0))*DLOG(DSQRT(2.0D0) - 1.0D0)

      DS = DS1 + AEX/(CS*R*Te)

C     CALCULO DE AS E BS

      BS = QS/(1.0D0 - DS)
      AS = R*Te*(QS*DS/(1.0D0 - DS))


C     ==================================================================
C
C                           CALCULO DE 'A' E 'B'
C
C     ==================================================================

      A = Pe*AS/((R*Te)**2)
      B = Pe*BS/(R*Te) 


C     ##################################################################
C
C     CALCULO DOS TERMOS RELATIVOS AOS COEFICIENTES DE FUGACIDADE 
C
C     ##################################################################


C     ==================================================================
C
C          CALCULO DOS LOGARITMOS DOS COEFICIENTES DE ATIVIDADE (DLCAT)
C
C     ==================================================================

      DLCAT(1) = DLOG(FRV(1)/COMP(1)) + (1.0D0 - 1.0D0/SEGMT)*FRV(2) + 
     +          PFR*(FRV(2)**2)
     
      DLCAT(2) = DLOG(FRV(2)/COMP(2)) + (1.0D0-SEGMT)*FRV(1)  
     +          + PFR*(FRV(1)**2)*SEGMT
   
C     ==================================================================
C
C     CALCULO DAS DERIVADAS PARCIAIS DE 'D' E 'Q' EM RELACAO AO NUMERO 
C     DE MOLES (DPDN(I),DPQN(I))
C
C     ==================================================================

      DO 700 I = 1,N
      	DPDN(I)=AI(I)/(BI(I)*R*Te) + DLCAT(I)/CS       	
 700  CONTINUE

      DO 800 I = 1,N      
        DO 750 J = 1,N
      	  DPQN1(I) = COMP(J)*((BI(I)+BI(J))/2.0D0-0.5D0*
     +               (AI(I)+AI(J))/(R*Te))*(1.0D0 - DELT(I,J))+
     +               DPQN1(I)
          DPQN(I) = 2.0D0*DPQN1(I)
 750   CONTINUE
 800  CONTINUE


C     ==================================================================
C
C     CALCULO DA DERIVADA PARCIAL DE 'AM' E 'BM' EM RELACAO AO NUMERO DE
C     MOLES (DPAM(I),DPBM(I))
C
C     ==================================================================

      DO 1100 I = 1,N
      	DPBM(I) = 1.0D0/(1.0D0 - DS)*DPQN(I) - QS/((1.0D0-DS)**2)*(1.0D0
     +            - DPDN(I))
      	DPAM(I) = R*Te*(DS*DPBM(I) + BS*DPDN(I))
 1100 CONTINUE
 

C     *******************
C     *******************      
      
      ELSE

C
C     *******************
C     *******************

      WRITE(*,*) 'PASSEI 1A'
      DO 2100 I=1,N
        DO 2000 J=1,N
          AS=AS+COMP(I)*COMP(J)*DSQRT(AI(I)*AI(J))*
     &       (1.0D0-DELT(I,J))
 2000   CONTINUE
        WRITE(*,*) 'PASSEI 1B'
        BS=COMP(I)*BI(I)
 2100 CONTINUE
 
      WRITE(*,*) 'PASSEI 2A'

      A = Pe*AS/((R*Te)**2)
      B = Pe*BS/(R*Te) 
      
      WRITE(*,*) 'PASSEI 2B'


C     *******************
C     *******************      
      
      ENDIF

C
C     *******************
C     *******************


 
       RETURN
       END



     
     



