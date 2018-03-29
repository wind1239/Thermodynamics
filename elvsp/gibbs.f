C     				ARQUIVO GIBBS.F


C     ==================================================================
C     ******************************************************************
C     ==================================================================      

      SUBROUTINE GIBBS(N,XOPT,GG,ITGG)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'common.f'

      DIMENSION XVOPT(ND),XOPT(ND),AUXL(ND),XFV(ND),XLOPT(ND)
      DIMENSION XFL(ND),XVTT(ND)
C     DOUBLE PRECISION GGM(1000000000000)


      IGIBBS = 1
      IGBS=IGBS+1
      
      BG=-GG


C     ==================================================================
C
C     AVALIACAO DOS RESULTADOS NA SAIDA DO ELV.  IDENTIFICACAO DA FASE 
C     DE SAIDA E DAS FRACOES MOLARES DOS COMPONENTES.
C
C     ==================================================================

      DO 600 I = 1,N-1
        YSOMA = XOPT(I) + YSOMA
 600  CONTINUE
      XMET = 1.0D0 - YSOMA   


C     ##################################################################
 
C     ************************    
      IF(XMET.GT.Ze(N))THEN
C     ************************

C       RESULTADOS : XL(I) I = 1,N-1 ; L ; FASE LIQUIDA

      	IF(XOPT(N).LT.0.50D0)THEN

C       ...........................
C       FASE VAPOR DOMINANTE
C       ...........................

      	  ELE = XOPT(N)
      	  V = 1.0D0 - ELE
 
      	ELSE

C       ............................
C     	FASE LIQUIDA DOMINANTE
C       ............................

      	  ELE = XOPT(N)
      	  V = 1.0D0 - ELE

      	ENDIF
      	                   	       		   	        		
      
C     ==================================================================
C
C                RESULTADOS EM FUNCAO DA FASE VAPOR 
C
C     ==================================================================
	
	XVOPT(N) = 1.0D0 - XOPT(N)
 
        DO 800 I = 1,N-1
           XVOPT(I) = (Ze(I) - XOPT(I)*XOPT(N))/XVOPT(N)
 800    CONTINUE

        XVSOMA = 0.0D0

        DO 850 I = 1,N-1
           XVSOMA = XVSOMA + XVOPT(I)
 850    CONTINUE
        XMETV = 1.0D0 - XVSOMA

        DO 930 I = 1,N-1
          XFV(I) = XVOPT(I)
 930    CONTINUE
        XFV(N) = XMETV

       
       CALL CONVW(N,XFV,COMP)  
            
C     ==================================================================
C
C     CALCULO DO PESO MOLECULAR MEDIO PARA IDENTIFICACAO OU CORRECAO DAS
C     FASES PRESENTES NA MISTURA.
C 
C     ==================================================================        
        
	MASSAV = 0.0D0
	MASSAL = 0.0D0
	
	DO 950 I = 1,N-1
	   MASSAL = XOPT(I)*MMOL(I) + MASSAL 
   	   MASSAV = XVOPT(I)*MMOL(I) + MASSAV
 950    CONTINUE
 
        MASSALF = XMET*MMOL(N)
        MASSAVF = XMETV*MMOL(N)   
        MASSALT = MASSAL + MASSALF
        MASSAVT = MASSAV + MASSAVF
         
        IF(MASSALT.GT.MASSAVT)THEN
          WRITE(31,951)XOPT(1)
          WRITE(32,951)ELE
          WRITE(33,951)BG
	  WRITE(34,952)IGBS
 951      FORMAT(F25.20)
 952      FORMAT(I20)
        ELSE
          AJUD1 = ELE
          V = AJUD1
          ELE = 1.0D0 - V
          AJUD2 = XMETV
          XMETL = AJUD2
          AJUD3 = XMET
          XVAPI = AJUD3
          DO 1000 I = 1,N-1
             AUXL(I) = XVOPT(I)
             XLOPT(I) = AUXL(I)
 1000     CONTINUE 

          WRITE(31,1001)XOPT(1)
          WRITE(32,1001)ELE
          WRITE(33,1001)BG
	  WRITE(34,1002)IGBS
 1001     FORMAT(F25.20)
 1002     FORMAT(I20)

  
      	ENDIF
 

C     ************************ 
      ELSE
C     ************************

 
C     	RESULTADOS : XV(I) I = 1,N-1 ; V ; FASE VAPOR
      	IF(XOPT(N).LT.0.5D0)THEN

C       ............................
C       FASE LIQUIDA DOMINANTE 
C       ............................

      	  V = XOPT(N)
      	  ELE = 1.0D0 - V

      	ELSE

C       ............................
C       FASE VAPOR DOMINANTE  
C       ............................
    	 
      	  V = XOPT(N)
      	  ELE = 1.0D0 - V

      	ENDIF               
      	
C     ==================================================================
C
C                  RESULTADOS EM FUNCAO DA FASE LIQUIDA 
C
C     ==================================================================
      	
        XLOPT(N) = 1.0D0 - XOPT(N)
 
        DO 1350 I = 1,N-1
           XLOPT(I) = (Ze(I) - XOPT(I)*XOPT(N))/XLOPT(N)
 1350   CONTINUE

 	XLSOMA = 0.0D0
 	
 	DO 1400 I = 1,N-1
 	   XLSOMA = XLOPT(I) + XLSOMA
 1400   CONTINUE
 
 	XMETL = 1.0D0 - XLSOMA

        DO 1455 I = 1,N-1
          XFL(I) = XLOPT(I)
 1455   CONTINUE
        XFL(N) = XMETL


       DO 1490 I =1,N-1
         XVT = XOPT(I) + XVT
 1490  CONTINUE
       XVT2 = 1.0D0 - XVT
    
       XVTT(1) = XOPT(1)
       XVTT(2) = XVT2
       
       CALL CONVW(N,XFL,COMP)    
        
C     ==================================================================
C
C     CALCULO DO PESO MOLECULAR MEDIO PARA IDENTIFICACAO OU CORRECAO DAS
C     FASES PRESENTES NA MISTURA.
C 
C     ==================================================================
        
        
	MASSAV = 0.0D0
	MASSAL = 0.0D0
	
	DO 1500 I = 1,N-1
	   MASSAL = XLOPT(I)*MMOL(I) + MASSAL 
   	   MASSAV = XOPT(I)*MMOL(I) + MASSAV
 1500   CONTINUE
 
        MASSALF = XMETL*MMOL(N)
        MASSAVF = XMET*MMOL(N)   
        MASSALT = MASSAL + MASSALF
        MASSAVT = MASSAV + MASSAVF
        
 
        IF(MASSALT.GT.MASSAVT)THEN
          WRITE(31,1501)XOPT(1)
          WRITE(32,1501)ELE
          WRITE(33,1501)BG
	  WRITE(34,1502)IGBS
 1501     FORMAT(F25.20)
 1502     FORMAT(I20)
        ELSE
          AJUD1 = ELE
          V = AJUD1
          ELE = 1.0D0 - V
          AJUD2 = XMETL
          XMETV = AJUD2
          AJUD3 = XMET
          XLIQU = AJUD3
          DO 1700 I = 1,N-1
             AUXL(I) = XLOPT(I)
             XVOPT(I) = AUXL(I)
 1700     CONTINUE     
          WRITE(31,1701)XOPT(1)
          WRITE(32,1701)ELE
          WRITE(33,1701)BG
	  WRITE(34,1702)IGBS
 1701     FORMAT(F25.20)
 1702     FORMAT(I20)

      	ENDIF
      	
      	
C     *********************
      ENDIF    
C     *********************





      RETURN

      END
