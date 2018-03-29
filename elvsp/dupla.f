
C     ******************************************************************
C     ******************************************************************

      SUBROUTINE DUPLA(ICASE,METHOD,INIT,COEF,ZVAP,ZLIQ,IDIVG)

C     ******************************************************************
C     ******************************************************************


C     ==================================================================
C
C     ESTA SUBROTINA RESOLVE A EQUACAO CUBICA EM 'Z', TENDO COMO REFEREN
C     CIA : HENDERSON, L. N. & TIEN, J. M. T. D. ; "ESTUDO COMPARATIVO 
C     DE METODOS NUMERICOS PARA A RESOLUCAO DE EQUACOES DE ESTADO" ;SE-
C     GUNDO WORKSHOP SOBRE APLICACOES DA CIENCIA NA ENGENHARIA DE PETRO-
C     LEO - SECAO ENGENHARIA DE RESERVATORIOS ; NOVEMBRO DE 1997.  SE EM
C     ULTIMA ANALISE HOUVER PROBLEMAS RESOLVER A EQUACAO CUBICA PELO ME-
C     TODO ANALITICO.
C
C     ==================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      DIMENSION COEF(4),ZHLP(3)

C     ==================================================================
C
C     PARA A EOS DE PENG-ROBINSON :
C
C     ==================================================================
C
C     Z**3 - (1 - B)*Z**2 + (A - 2*B -3*B**2)*Z - (A*B - B**3 - B**2) = 0
C
C     F(Z) = P3*Z**3 - P2*Z**2 + P1*Z - P0
C
C
C     COEF(1) = A*B - B**2 - B**3 = P0
C     COEF(2) = A - 2*B - 3*B**2 = P1
C     COEF(3) = 1 - B = P2
C     COEF(4) = B
C     P3 = 1.0D0

      WRITE(77,*)'B : ',COEF(4)

      GOTO 7000
        
      P3 = 1.0D0
      P2 = COEF(3)
      P1 = COEF(2)          
      P0 = COEF(1)
      BN = COEF(4)

      
C     ==================================================================
C
C               ESTIMATIVA INICIAL PARA Z (LIQUIDO E VAPOR)  
C
C     ==================================================================


C     ------------------------------------------------------------------
C
C                                FASE VAPOR            
C
C     ------------------------------------------------------------------
      
      IF(INIT.EQ.1)THEN
        Z0STAR = P2 - P1/P2 - P0/(P2**3 - P1)
        IF(Z0STAR.LE.BN)THEN
          Z0 = DMAX1(1.0D0,BN)
        ELSE
          Z0 = Z0STAR
        ENDIF
        
C     ------------------------------------------------------------------
C
C                              FASE LIQUIDA            
C
C     ------------------------------------------------------------------
        
      ELSE
      	Z0STAR = P1*BN/P0 - P2*BN/P1 + BN**2/(P1*(P1*BN/P0 - P2*BN/P1))
      	Z0 = DMAX1(1.0D0,Z0STAR)
      	
      ENDIF

C     WRITE(77,*)'Z0 : ',Z0
      

C     ------------------------------------------------------------------
      
      Z = Z0
      ZN = Z0
      ZL = Z0
      NF = 1000000
      ITNMX = 100000
      ERZ = 1.0D-5
      
      
C     ==================================================================
C
C     SERA IMPLEMENTADO O METODO DE SCHNABEL E FRANK MODIFICADO, MAS AN-
C     TES SERA FEITO UMA AVALIACAO PRELIMINAR PELO METODO DE NEWTON.
C
C     ==================================================================
      
C     ------------------------------------------------------------------
C
C                           METODO DE NEWTON           
C
C     ------------------------------------------------------------------

C     WRITE(77,*)'NEWTON '

      DO 20 ITERZ = 1,NF
            
      	FNCZ = FCZ(P0,P1,P2,P3,BN,ZN,INIT)
      	D1FZ = D1Z(P0,P1,P2,P3,BN,ZN,INIT)
      	      
      	ZNEW = ZN - FNCZ/D1FZ  	     
        ERFZN = (ZNEW - ZN)/ZN
      	
      	IF((ZNEW.LT.1.0D0).AND.(ZNEW.GT.0.0D0).AND.
     +               (ITERZ.GT.1).AND.(DABS(ERFZN).LT.ERZ))THEN
C     	  WRITE(77,15)ITERZ,ZNEW
 15       FORMAT(/,'ITERACAO : ',I10,/,'ZNEWTON : ',D25.13)
          GOTO 8000
        ELSEIF(ITERZ.GE.ITNMX)THEN
C         WRITE(77,18)ITERZ,ZNEW
 18       FORMAT(/,'ITERACAO : ',I10,/,'ZNEWTON : ',D25.13) 
C         WRITE(77,*)'PASSANDO PARA O METODO HIBRIDO'
C         WRITE(77,*)' '         
        ELSE
          ZN = ZNEW
        ENDIF 

 20   CONTINUE
 
 2000 CONTINUE
 
 
C     ------------------------------------------------------------------
C
C            METODO SCHNABEL & FRANK MODIFICADO (METODO HIBRIDO)
C
C     ------------------------------------------------------------------
      
C     WRITE(77,*)'SCHNABEL & FRANK'

      FNCZ = 0.0D0
      D1FZ = 0.0D0
      D2FZ = 0.0D0
      D3FZ = 0.0D0
      ZNEW = 0.0D0  
      	
      Z = ZL
      	
      FNCZ = FCZ(P0,P1,P2,P3,BN,Z,INIT)
      D1FZ = D1Z(P0,P1,P2,P3,BN,Z,INIT)
      D2FZ = D2Z(P0,P1,P2,P3,BN,Z,INIT)
      D3FZ = D3Z(P0,P1,P2,P3,BN,Z,INIT)
      
      DLT1Z = DLT1(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      DLT2Z = DLT2(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)            	          
      
      IF(DLT1Z.GT.0.0D0)THEN
      	
      	  DO 100 INTR = 1,NF  
      	     IF(INTR.GT.1)THEN
      	       FNCZ = FCZ(P0,P1,P2,P3,BN,Z,INIT)
      	       D1FZ = D1Z(P0,P1,P2,P3,BN,Z,INIT)
      	       D2FZ = D2Z(P0,P1,P2,P3,BN,Z,INIT)
      	       D3FZ = D3Z(P0,P1,P2,P3,BN,Z,INIT)    
      	       DLT1Z = DLT1(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      	       DLT2Z = DLT2(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      	     ENDIF            	             	       	
      	     ZNEW = Z - 2.0D0*FNCZ/(D1FZ + DSIGNF(D1FZ)*DSQRT(DLT2Z))	     
      	     ERFZ = (ZNEW - Z)/Z
      	     FNCZN = FCZ(P0,P1,P2,P3,BN,ZNEW,INIT)
      	     IF((INTR.GE.2).AND.(DABS(ERFZ).LT.ERZ).AND.
     +         (ZNEW.LT.1.0D0).AND.(ZNEW.GT.0.0D0))THEN
C     	       WRITE(77,50)INTR,ZNEW
 50            FORMAT(/,'ITERACAO : ',I10,/,'ZSF : ',D25.13)
               PRINT*,'Z : ',ZNEW,' NA ITERACAO  ',INTR
               GOTO 8000
             ELSE
               Z = ZNEW
             ENDIF
 100      CONTINUE
C         WRITE(77,*)'PASSANDO PARA O METODO DE EDMISTER & LEE'
C         WRITE(77,*)' '
          
       ELSE 
          
C     ------------------------------------------------------------------
C
C       OPTANDO PELA MODIFICACAO DO METODO DE EDMISTER & LEE          
C
C     ------------------------------------------------------------------
          
        WRITE(*,*)'EDMISTER & LEE '

        IF(INIT.EQ.1)THEN
          PP = 1.0D0
        ELSE
          PP = 1.0D0/(BN**2)
        ENDIF
  
        DO 200 INTR = 1,NF
      	   ZNEW = Z - FNCZ/(D1FZ - FNCZ*D2FZ/(2.0D0*D1FZ) + 
     +            PP*(FNCZ/D1FZ))
           ERFZ = (ZNEW - Z)/Z
      	   IF((INTR.GE.2).AND.(DABS(ERFZ).LT.ERZ).AND.
     +         (ZNEW.GT.0.0D0).AND.(ZNEW.LT.1.0D0))THEN
C      	     WRITE(77,150)INTR,ZNEW
 150         FORMAT(/,'ITERACAO : ',I10,/,'ZEL : ',D25.13)   	     
             GOTO 8000
           ELSEIF(INTR.GT.ITNMX)THEN
             WRITE(77,160)INTR
 160         FORMAT(/,I10,' ITERACOES')
C            WRITE(77,*)'DELTA MENOR QUE ZERO'
C            WRITE(77,*)'OUTRO METODO'
C            WRITE(77,*)' '
             GOTO 5000
           ELSE             
             Z = ZNEW
           ENDIF
 200    CONTINUE
 
      ENDIF
        
 5000   CONTINUE
 
        Z = 0.0D0
        Z = ZL
        
        FNCZ = 0.0D0
      	D1FZ = 0.0D0
      	D2FZ = 0.0D0
      	D3FZ = 0.0D0
      	ZNEW = 0.0D0  
      	

 	DO 400 INTR1 = 1,NF
 	
 	   FNCZ = FCZ(P0,P1,P2,P3,BN,Z,INIT)
 	   FNCZ1 = FNCZ
      	   D1FZ = D1Z(P0,P1,P2,P3,BN,Z,INIT)
      	   D2FZ = D2Z(P0,P1,P2,P3,BN,Z,INIT)
      	   D3FZ = D3Z(P0,P1,P2,P3,BN,Z,INIT)      
      	   DLT1Z = DLT1(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      	   DLT2Z = DLT2(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)      
           
           IF(INIT.EQ.1)THEN
             TK = D2FZ - 2.0D0*FNCZ/D1FZ
           ELSE
             TK = D2FZ - 2.0D0*FNCZ/((BN**2)*D1FZ)
           ENDIF      	        
 
 	   ZNEW = Z - FNCZ/TK
 	   ERFZ = (ZNEW - Z)/Z     	
  	   IF((INTR1.GE.2).AND.(DABS(ERFZ).LT.ERZ).AND.
     +         (ZNEW.GT.0.0D0).AND.(ZNEW.LT.1.0D0))THEN
 	     Z = ZNEW    
 	     FNCZ = FCZ(P0,P1,P2,P3,BN,Z,INIT)
      	     D1FZ = D1Z(P0,P1,P2,P3,BN,Z,INIT)
      	     D2FZ = D2Z(P0,P1,P2,P3,BN,Z,INIT)
      	     D3FZ = D3Z(P0,P1,P2,P3,BN,Z,INIT)      
      	     DLT1Z = DLT1(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      	     DLT2Z = DLT2(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      	     DMC = DMCK(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      	     IF(DABS(DMC).LT.(0.5D0*DABS(FNCZ1)))THEN
C      	       WRITE(77,250)INTR1,ZNEW
 250           FORMAT(/,'ITERACAO : ',I10,/,'ZOT1 : ',D25.13)
               GOTO 8000
             ELSEIF(INTR1.GT.ITNMX)THEN
C              WRITE(77,260)INTR1
 260           FORMAT(/,I10,' ITERACOES !!!')
C              WRITE(77,*)'METODO ANALITICO '
C              WRITE(77,*)' '
               GOTO 7000
             ENDIF
           ELSE             
               Z = ZNEW
           ENDIF
 400   	CONTINUE      


 7000 CONTINUE

C     ======================================================================
C     ======================================================================
C     ======================================================================
C
C                               METODO ANALITICO       
C
C     ======================================================================
C     ======================================================================
C     ======================================================================

       WRITE(77,*)'METODO ANALITICO '
       
        P1 = -COEF(3)
	P2 = COEF(2)
	P3 = -COEF(1)
        BZ = COEF(4)

C	CALCULO DE QC, RC E DC :

	QC = (3.0D0*P2-P1**2)/9.0D0
	RC = (9.0D0*P1*P2-27.0D0*P3-2.0D0*P1**3)/54.0D0
	DC = QC**3+RC**2

c        SC = (RC+DSQRT(DC))**(1/3)
c	TC1 = (RC-DSQRT(DC))**(1/3)

	IF(DC.LT.0.0D0)THEN
C	  WRITE(77,*)'CUBICA: 3 RAIZES REAIS E DISTINTAS'
	  THETAC = DACOS(RC/DSQRT(-QC**3))
	  ZC1 = 2.0D0*DSQRT(-QC)*DCOS(THETAC/3.0D0)-P1/3.0D0
	  ZC2 = 2.0D0*DSQRT(-QC)*DCOS(THETAC/3.0D0 + 120.0D0)-P1/3.0D0
          ZC3 = 2.0D0*DSQRT(-QC)*DCOS(THETAC/3.0D0 + 240.0D0)-P1/3.0D0
	ELSEIF(DC.GT.0.0D0)THEN
C         WRITE(77,*)'CUBICA : 1 RAIZ REAL E 2 IMAGINARIAS'
	  SC = (RC+SQRT(DC))**(1/3)
	  TC1 = (RC-SQRT(DC))**(1/3)
          ZC1 = SC+TC1-P1/3.0D0
	  ZC2 = 0.0D0
          ZC3 = 0.0D0
	ELSE
C	  WRITE(77,*)'CUBICA : RAIZES REAIS E PELO MENOS 2 SAO IGUAIS'	  
          SC = (RC)**(1/3)
          TC1 = (RC)**(1/3)
          ZC1 = SC+TC1-P1/3.0D0
          ZC2 = -(SC+TC1)/2.0D0 - P1/3.0D0
          ZC3 = ZC2
	ENDIF

C	INIT = 1 : FASE VAPOR (MAIOR VALOR REAL E POSITIVO DE Z)
C	INIT = 2 : FASE LIQUIDA (MENOR VALOR REAL E NEGATIVO DE Z)

C       ==============================================================
C
C       ELIMINANDO VALORES DE 'Z' MAIORES QUE UM E MENORES QUE ZERO
C       E ORDENANDO-OS
C
C       ==============================================================  

        ZHLP(1) = ZC1
        ZHLP(2) = ZC2
        ZHLP(3) = ZC3

        ZHP0 = 0.0D0
        ZHP1 = 1.0D0
        NZ = 3
        ZMIN =  -1.0D6
        ZMAX = 1.0D6

        DO 405 I = 1,NZ
C         WRITE(77,*)'Z(',I,') : ',ZHLP(I)
405     CONTINUE 

        ZMIN1 = DMIN1(ZC1,ZC2,ZC3)
        ZMAX1 = DMAX1(ZC1,ZC2,ZC3)
     
       	IF(INIT.EQ.1)THEN
          DO 410 I = 1,NZ
            IF((ZHLP(I).GT.ZMIN).AND.(ZHLP(I).GT.ZHP0).AND.
     &       			(ZHLP(I).LT.ZHP1))THEN
              ZMIN = ZHLP(I)
            ENDIF
 410      CONTINUE
	  ZVAP1 = ZMIN
          ZLIQ = DMIN1(ZC1,ZC2,ZC3)
          IF((ZVAP1.LE.BZ).OR.(ZVAP1.LT.ZHP0).OR.
     &       			(ZVAP1.GT.ZHP1))THEN 
            ZVAP = Z0
            ENUM = ENUM + 1
          ELSE
             ZVAP = ZVAP1
          ENDIF
       	ELSE
	  DO 420 I = 1,NZ
            IF((ZHLP(I).LT.ZMAX).AND.(ZHLP(I).GT.ZHP0).AND.
     &       			(ZHLP(I).LT.ZHP1))THEN
              ZMAX = ZHLP(I)
            ENDIF
 420      CONTINUE
          ZLIQ1 = ZMAX
          ZVAP = DMAX1(ZC1,ZC2,ZC3)
          IF((ZLIQ1.LE.BZ).OR.(ZLIQ1.LT.ZHP0).OR.
     &       			(ZLIQ1.GT.ZHP1))THEN
             ZLIQ = Z0
             ENUM = ENUM + 1
          ELSE
             ZLIQ = ZLIQ1
          ENDIF
       	ENDIF

C       WRITE(77,*)'ZMAX : ',ZMAX1
C       WRITE(77,*)'ZMIN : ',ZMIN1
C       WRITE(77,*)'INIT : ',INIT
C       WRITE(77,*)'ZLIQ(',ITERZZ,') : ',ZLIQ
C       WRITE(77,*)'ZVAP(',ITERZZ,') : ',ZVAP
C       WRITE(77,*)'ENUM : ',ENUM
        IDIVG=1
        ITERZZ = ITERZZ + 1
        GOTO 9000
 	          
 8000 CONTINUE
 
      IF(INIT.EQ.1)THEN
      	ZVAP = ZNEW
        ZLIQ = 1.0D-6
C     	WRITE(77,450)ZVAP
 450   	FORMAT(/,'ZVAP : ',D25.13)
      ELSE
      	ZLIQ = BN/ZNEW
        ZVAP = 1.0D-6
C     	WRITE(77,480)ZLIQ
 480   	FORMAT(/,'ZLIQ : ',D25.13)
      ENDIF
      IDIVG=1

 9000 CONTINUE
      

      RETURN
      END
       	                      	      
       	                      	      
C     ==================================================================
C
C                  CRIACAO DOS POLINOMIOS DE TRABALHO 
C
C     ==================================================================

      FUNCTION FCZ(P0,P1,P2,P3,BN,Z,INIT)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      
      IF(INIT.EQ.1)THEN      
      	FCZ = P3*(Z**3) - P2*(Z**2) + P1*Z - P0
      ELSE
	Z = BN/Z
      	H1 = P0/(BN**2)
      	H2 = P1/BN
      	FCZ = H1*(Z**3)- H2*(Z**2) + P2*Z - BN
      ENDIF
      
      RETURN
      END
      
C     ------------------------------------------------------------------          

      FUNCTION D1Z(P0,P1,P2,P3,BN,Z,INIT)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      IF(INIT.EQ.1)THEN
      	D1Z = 3.0D0*P3*(Z**2) - 2.0D0*P2*Z + P1
      ELSE
	Z = BN/Z
      	H1 = P0/(BN**2)
      	H2 = P1/BN
      	D1Z = 3.0D0*H1*(Z**2)- 2.0D0*H2*Z + P2
      ENDIF
      
      RETURN
      END
      
C     ------------------------------------------------------------------          

      FUNCTION D2Z(P0,P1,P2,P3,BN,Z,INIT)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      IF(INIT.EQ.1)THEN
      	D2Z = 6.0D0*P3*Z - 2.0D0*P2 
      ELSE
	Z = BN/Z
      	H1 = P0/(BN**2)
      	H2 = P1/BN
      	D2Z = 6.0D0*H1*Z - 2.0D0*H2
      ENDIF
      
      RETURN
      END      	    	      	      	
      	      
C     ------------------------------------------------------------------          

      FUNCTION D3Z(P0,P1,P2,P3,BN,Z,INIT)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      IF(INIT.EQ.1)THEN
  	D3Z = 6.0D0*P3
      ELSE
        Z = BN/Z
      	H1 = P0/(BN**2)
      	H2 = P1/BN
      	D3Z = 6.0D0*H1
      ENDIF
      
      RETURN
      END      
      
C     ------------------------------------------------------------------          

      FUNCTION DLT1(FNCZ,D1FZ,D2FZ,D3FZ,Z)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IF(INIT.EQ.1)THEN
        TK = D2FZ - 2.0D0*FNCZ/D1FZ
      ELSE
        TK = D1FZ - 2.0D0*FNCZ/((BN**2)*D1FZ)
      ENDIF
      
      DLT1 = D1FZ**2 - 2.0D0*TK*FNCZ
     
      RETURN
      END
      
C     ------------------------------------------------------------------                
      
      FUNCTION DLT2(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IF(INIT.EQ.1)THEN
        TK = D2FZ - 2.0D0*FNCZ/D1FZ
      ELSE
        TK = D1FZ - 2.0D0*FNCZ/((BN**2)*D1FZ)
      ENDIF
      
      DLT2 = D1FZ**2 - 2.0D0*TK*FNCZ
     
      RETURN
      END      
      
C     ------------------------------------------------------------------

      FUNCTION DMCK(FNCZ,D1FZ,D2FZ,D3FZ,BN,Z,INIT)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IF(INIT.EQ.1)THEN
        TK = D2FZ - 2.0D0*FNCZ/D1FZ
      ELSE
        TK = D1FZ - 2.0D0*FNCZ/((BN**2)*D1FZ)
      ENDIF

      DMIN = -FNCZ/TK
      
      DMCK = FNCZ + D1FZ*DMIN +5.0D-1*TK*(DMIN**2)
     
      RETURN
      END    

C     ------------------------------------------------------------------

      FUNCTION DSIGNF(FX)
      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
      
      DSIGNF = 1.0D0  
      IF(FX.LT.0.0D0)DSIGNF = -1.0D0 
      
      RETURN
      END      		

C     ------------------------------------------------------------------					
