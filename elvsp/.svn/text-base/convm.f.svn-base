C      ARQUIVO CONVM.F


C      ==============================================================
C
       SUBROUTINE CONVM(N,XXCV,WCV)
C
C      ==============================================================

C      ROTINA PARA CONVERSAO DE UNIDADES DE CONCENTRACAO : FRACAO MO-
C      LAR EM FRACAO MASSICA

C      --------------------------------------------------------------
C
C      		   DECLARACAO E DEFINICAO DAS VARIAVEIS 
C
C      -------------------------------------------------------------- 

       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'common.f'
       INCLUDE 'comcalc.f'

       DOUBLE PRECISION XCV(ND),WCV(ND)
        

       SUM1 = 0.0D0
       DO 50 I = 1,N
         SUM1 = XCV(I)*MMOL(I) + SUM1
 50    CONTINUE
      
       SUM2 = 0.0D0 
       DO 100 I = 1,N-1
         WCV(I) = XCV(I)*MMOL(I)/SUM1
         SUM2 = WCV(I) + SUM2
 100   CONTINUE

       WCV(N) = 1.0D0 - SUM2
 
       RETURN

       END

         
       
