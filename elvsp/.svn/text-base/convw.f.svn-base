C      ARQUIVO CONVW.F


C      ==============================================================
C
        SUBROUTINE CONVW(N,WCV,XCV)
C
C      ==============================================================

C      		ROTINA PARA CONVERSAO DE UNIDADES DE CONCENTRACAO : 
C			FRACAO MASSICA EM FRACAO MOLAR

C      --------------------------------------------------------------
C
C      		   DECLARACAO E DEFINICAO DAS VARIAVEIS 
C
C      -------------------------------------------------------------- 

       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'common.f'
       INCLUDE 'comcalc.f'
  
       DOUBLE PRECISION ACV(ND,ND),BCV(ND),WCV(ND)
       DOUBLE PRECISION XCV(ND),XCVN(ND),ERP(ND),SUM1,SUM2
       DOUBLE PRECISION XEST,EPS1,MAXEVL1
       INTEGER I,J,N,K,NCP

C      				MATRIZ DE TRABALHO 
C
C      DIAGONAL PRINCIPAL : 
C		MMOL(I)*(WCV(I) - 1) - MMOL(N)*WCV(I)
C      NAS LINHA "I" E COLUNA "J" :
C		MMOL(J)*WCV(I) - MMOL(N)*WCV(I)
C
C      LEMBRAR QUE "J = 1,N-1"
C
C
C      				VETOR RESPOSTA
C
C	-XCV(I)*MMOL(N)
C
C
C

C      ...............................................................
C
C       ESTABELECENDO OS VALORES DA ESTIMATIVA INICIAL, NUMERO MAXIMO 
C       DE AVALIACOES E ERRO RELATIVO PARA IMPLEMENTACAO  POSTERIOR DO
C       METODO ITERATIVO DE GAUSS-SEIDL

        XEST = 0.005
        MAXEVL1 = 100000000
        MAXEVL=100000000
        EPS1 = 1.0D-8

C
C      ...............................................................

C      ---------------------------------------------------------------
C
C      		CONSTRUCAO DA MATRIZ A E DO VETOR RESPOSTA
C
C      ---------------------------------------------------------------


       DO 150 I = 1,N-1
         BCV(I) = -WCV(I)*MMOL(N)
         DO 100 J = 1,N-1
           IF(I.EQ.J)THEN
             ACV(I,J) = MMOL(I)*(WCV(I) - 1) - MMOL(N)*WCV(I)
           ELSE
             ACV(I,J) = MMOL(J)*WCV(I) - MMOL(N)*WCV(I)
           ENDIF
 100     CONTINUE
 150   CONTINUE

C      ---------------------------------------------------------------
C
C          ESTIMATIVA INICIAL DO VETOR DE VARIAVEIS INDEPENDENTES
C
C      ---------------------------------------------------------------

       XCVT = 0.0D0
       DO 200 I = 1,N-1
         XCV(I) = XEST
         XCVN(I) = XEST
         XCVT = XCV(I) +XCVT
 200   CONTINUE

       XCV(N) = 1.0D0 - XCVT
       XCVN(N) = 1.0D0 - XCVT

C      ..............................................................
C
C      PARA RESOLVER O SISTEMA LINEAR SERAH APLICADO O METODO ITERATI
C      VO DE GAUSS-SEIDEL
C
C      ..............................................................

       DO 500 K = 1,MAXEVL1

         DO 400 I = 1,N-1

           SUM1 = 0.0D0
           SUM2 = 0.0D0

           IF(I.GT.1)THEN
             DO 300 J = 1,I-1
               SUM1 = ACV(I,J)*XCVN(J) + SUM1
 300         CONTINUE
           ENDIF

           DO 350 J = I+1,N-1
             SUM2 = ACV(I,J)*XCV(J) + SUM2
 350       CONTINUE

           XCVN(I) = 1/ACV(I,I)*(BCV(I) - SUM1 - SUM2)

 400     CONTINUE

 
C      -------------------------------------------------------
C
C    			CRITERIO DE PARADA
C 
C      -------------------------------------------------------

         DO 450 NCP = 1,N-1
           ERP(NCP) = (XCVN(NCP)-XCV(NCP))/XCV(NCP)
           IF(ERP(NCP).LE.EPS1)THEN
             IF(NCP.LT.(N-1))THEN
               GOTO 450
             ELSE
               GOTO 550
             ENDIF
           ELSE
             GOTO 460
           ENDIF
 450     CONTINUE
 460     CONTINUE

         IF(K.EQ.MAXEVL)THEN
           WRITE(*,*)MAXEVL,' AVALIACOES. TRY AGAIN !!!'
           STOP
         ENDIF
         
         SUMX = 0.0D0
         DO 470 I = 1,N-1
           SUMX = XCVN(I) + SUMX
           XCV(I) = XCVN(I)
 470     CONTINUE
       
          XCVN(N) = 1.0D0 - SUMX
          XCV(N) = 1.0D0 - SUMX

 500    CONTINUE

 550    CONTINUE

        SUMXN = 0.0D0
        DO 600 I = 1,N-1
          SUMXN = XCVN(I) + SUMXN
 600    CONTINUE
        XCVN(N) = 1.0D0 - SUMXN

        RETURN
        END         

        

        


         

	       
       
       




       

