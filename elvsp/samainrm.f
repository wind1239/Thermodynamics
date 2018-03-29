
C     ARQUIVO SAMAINRM.F

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'		


      PARAMETER (NEPS = 4)
      CHARACTER*15 ARQ1,ARQ2,ARQ3,ARQ4
      DOUBLE PRECISION LB,MASSAL,MASSAV,MASSAVF,MASSALF,MASSAVT,MASSALT,
     &                AJUD1,AJUD2,AJUD3,XVAPI, XLIQU,DELT1	
      INTEGER  NACP, NS, NT, NFCNEV, IER, ISEED1, ISEED2,
     &         MAXEVL, IPRINT, NACC, NOBDS, N 
      LOGICAL  MAX
      EXTERNAL FCN

      DIMENSION  LB(ND),UB(ND),X(ND),XOPT(ND),C(ND),VM(ND),
     &           FSTAR(NEPS),XP(ND),XV(ND),XL(ND),GL(ND), 
     &           GV(ND),C1(ND),VM1(ND),XX(ND),XLOPT(ND),
     &	     	 XVOPT(ND),CA(ND),AUXL(ND),NACP(ND),XFV(ND),
     &           XFL(ND),COMP(ND),XVTT(ND),ATIVP(ND)        		     		   		  	


C     ******************************************************************
C     ******************************************************************
C
C     LEITURA DO NOME DE ARQUIVOS DE DADOS
C
C     ******************************************************************
C     ******************************************************************
    
      READ(5,1) ARQ1
      READ(5,1) ARQ2
      READ(5,1) ARQ3
      READ(5,1) ARQ4
1     FORMAT(A15)


C     ******************************************************************
C     ******************************************************************
C
C           LEITURA DOS PARAMETROS DE ENTRADA PARA O ALGORITMO  
C			   SIMULATED ANNEALING
C
C     ******************************************************************
C     ******************************************************************


      OPEN(3,FILE=ARQ1,STATUS='UNKNOWN')	
      READ(3,*) MAX
      READ(3,*) EPS
      READ(3,*) RT
      READ(3,*) ISEED1
      READ(3,*) ISEED2
      READ(3,*) NS
      READ(3,*) NT 
      READ(3,*) MAXEVL 
      READ(3,*) IPRINT
      READ(3,*) T
      READ(3,*) N      	
      DO 10, I = 1, N 
          READ(3,*) LB(I)
          READ(3,*) UB(I)	 
          READ(3,*) C(I)
          READ(3,*) VM(I)
	  C1(I)=C(I)
	  VM1(I)=VM(I)	
 10   CONTINUE
      RT1 = RT
      NS1 = NS
      NT1 = NT
      MAXEVL1 = MAXEVL
      T1 = T

C     ******************************************************************
C     ******************************************************************
C
C           LEITURA DOS DADOS DE ENTRADA REFERENTE AOS COMPONENTES
C	   			DA MISTURA
C
C     1. VETOR PESO MOLECULAR
C     2. CONSTANTE UNIVERSAL DOS GASES (KPa*m^3/(mol*K))
C     3. TEMPERATURA (K)
C     4a. PRESSAO INICIAL (KPa)
C     4b. PRESSAO FINAL (KPa)
C     4c. DELTA DE PRESSAO (KPa)
C     5. DIMENSAO DO SEGMENTO
C     6. PARAMETRO DE FLORY-HUGGINS
C     7. VETOR DE ALIMENTACAO : FRACAO MASSICA
C     8. FATOR ACENTRICO DO SOLVENTE
C     9. PARAMETRO K1 DO SOLVENTE
C    10. PRESSAO CRITICA DO SOLVENTE (KPa)
C    11. TEMPERATURA CRITICA DO SOLVENTE (K)
C    11a. TEMPERATURA DE EBULICAO DO SOLVENTE (K)
C    12. AI DO SOLUTO (KPa*m^6/mol^2)
C    13. BI DO SOLUTO (m^3/mol)
C    14. MATRIZ PARAMETRO DE INTERACAO BINARIA (I,J)
C    15. PARAMETRO "A" DO CALCULO DA PRESSAO DE VAPOR (METODO WAGNER)
C    16. PARAMETRO "B" DO CALCULO DA PRESSAO DE VAPOR (METODO WAGNER)
C    17. PARAMETRO "C" DO CALCULO DA PRESSAO DE VAPOR (METODO WAGNER)
C    18. PARAMETRO "D" DO CALCULO DA PRESSAO DE VAPOR (METODO WAGNER)
C    19. PARAMETRO PARA VARREDURA - IOPT ; COM VARREDURA (1) E SEM VARREDURA (0)
C    20. PARAMETRO PARA OPCAO DE EOS - IEOS ; PRSV (1) E PR (0)
C    21. PARAMETRO PARA OPCAO DO CALCULO DA ATIVIDADE - IATV :
C                (1) EQ. FLORY-HUGGINS ; (0) EQ. ANALITICA
C    22. PARAMETRO PARA OPCAO DA REGRA DE MISTURA - IRM :
C                (1) WONG-SANDLER ; (0) CLASSICA  
C    23. PARAMETRO PARA OPCAO DO TIPO DE SOLVENTE PARA O CALCULO DA PRESSAO
C         DE VAPOR - ISOLV : (0) NON-POLAR ; (1) POLAR ; (2) AGUA E ALCOOIS  
C
C
C
C     ******************************************************************
C     ******************************************************************


      OPEN(2,FILE=ARQ2,STATUS='UNKNOWN')
           
      DO 12 I = 1,N
	READ(2,*)MMOL(I)
 12   CONTINUE
 
      READ(2,*)R
      READ(2,*)Te	
      READ(2,*)Pe
      READ(2,*)Pf
      READ(2,*)Pd
      READ(2,*)SEGMT
      READ(2,*)PFR

      READ(2,*)Ze(1)
      Ze(2)=1.0D0-Ze(1)
               
      READ(2,*)W(1)
      READ(2,*)DK1(1)
      READ(2,*)Pc(1)
      READ(2,*)Tc(1)
      READ(2,*)Tb(1)
      READ(2,*)AI(2)
      READ(2,*)BI(2)

      READ(2,*)XV(1)
      READ(2,*)XL(1)
      READ(2,*)ELE

      XV(2)=1.0D0-XV(1)
      XL(2)=1.0D0-XL(1)
      V=1.0D0-ELE

      READ(2,*)DELT1

      DO 46 I = 1,N
 	DELT(I,I)=0.0D0
  46  CONTINUE	

      DELT(1,2)=DELT1
      DELT(2,1)=DELT(1,2)

      READ(2,*)APV
      READ(2,*)BPV
      READ(2,*)CPV
      READ(2,*)DPV
      READ(2,*)IOPC
      READ(2,*)IEOS 
      READ(2,*)IATV
      READ(2,*)IRM 
      READ(2,*)ISOLV


      OPEN(57,FILE='LISTA.OUT',STATUS='UNKNOWN')
      OPEN(31,FILE='xlm.m',STATUS='UNKNOWN')
      OPEN(32,FILE='lm.m',STATUS='UNKNOWN')
      OPEN(33,FILE='ggm.m',STATUS='UNKNOWN')
      OPEN(34,FILE='ind.m',STATUS='UNKNOWN')

      WRITE(*,*)'Pe : ',Pe
      WRITE(*,*)'Pf : ',Pf
      WRITE(*,*)'Pd : ',Pd
      WRITE(*,*)' '

      WRITE(57,*)'T : ',Te,' K'
      WRITE(57,*)'Pi : ',Pe,' KPa'
      WRITE(57,*)'Pf : ',Pf,' KPa'
      WRITE(57,*)'Ze(1) : ',Ze(1)

      WRITE(57,*)'    Pressao(KPa)                ATIVP(1)'

C     ====================================================================
C     ********************************************************************
C     ====================================================================
C
C     SERAH FEITO UMA VARREDURA COM DIVERSOS VALORES DE PRESSAO
C
C     ====================================================================
C     ********************************************************************
C     ====================================================================
   
      Tr(1) = Te/Tc(1)
      IAVAL = 1
 7777 CONTINUE
      
      Pr(1) = Pe/Pc(1)
      
  
C     ##################################################################
C
C     INICIALMENTE SERAO USADOS OS DADOS DE AI(I) E BI(I) OBTIDOS DIRETA
C     MENTE DO PAPER DE 'ORBEY & SANDLER' (1994). ESTAO SENDO VERIFICA-
C     DOS NOVAS ROTINAS PARA O CALCULO DAS CONSTANTES A SEREM USADAS NO
C     LUGAR DAS CONSTANTES CRITICAS PARA IMPLEMENTACAO DO MODELO SUGERI-
C     POR 'STRYJEK & VERA' (1986).
C
C     ##################################################################

                             CALL CALC(N,IEOS)     

C     ##################################################################
C
C     ESCOLHA DA CONFIGURACAO MAIS ADEQUADA DA ENERGIA LIVRE DE GIBBS MO
C     LAR DAS FASES LIQUIDA ('GL') E VAPOR ('GV')
C
C     ##################################################################
		                          
      OPEN(10,FILE = ARQ3,STATUS='UNKNOWN')
      
      ALFAV = 0.0D0
      ALFAL =0.0D0
      
      DO 55 I = 1,N
      	ALFAV = ((Ze(I) - XV(I))**2) + ALFAV
      	ALFAL = ((Ze(I) - XL(I))**2) + ALFAL
 55   CONTINUE
 
      ALFAV = DSQRT(ALFAV)
      ALFAL = DSQRT(ALFAL)
      
      WRITE(*,*)'ALFAV : ',ALFAV
      WRITE(*,*)'ALFAL : ',ALFAL
      
      
C     ##################################################################
C
C                  COMPARACAO PARA ESCOLHA DAS FASES
C
C     ##################################################################
      

      IF(ALFAL.GT.ALFAV)THEN
      	CALL MILCONSRM(N,IER6,GL)
      	DO 80 I = 1,N
      	  X(I) = XL(I)
      	  XX(I) = X(I)
      	  GAMA1(I) = GL(I)
 80   	CONTINUE
 	X(N) = ELE
 	XX(N) = X(N)
      ELSE
      	CALL MIVCONSRM(N,IER6,GV)
      	DO 90 I = 1,N
      	  X(I) = XV(I)
          XX(I) = X(I)
      	  GAMA1(I) = GV(I)
 90	CONTINUE
 	X(N) = V
 	XX(N) = X(N)
      ENDIF
      
      GZERO = 0.0D0
      DO 100 I = 1,N
      	GZERO = (Ze(I)*GAMA1(I)) + GZERO
 100  CONTINUE

C     IMPRESSAO DA COMPOSICAO DA CORRENTE DE ALIMENTACAO E DA PRESSAO

      DO 110 I = 1,N
      	WRITE(10,*)'WCV(',I,') : ',Ze(I)
 110  CONTINUE
 
      WRITE(10,*)'PRESSAO : ',Pe,' KPa'
      WRITE(10,*)'TEMPERATURA : ',Te,' K'


      
C     ******************************************************************
C     ******************************************************************
C
C     			   TESTE DE ESTABILIDADE
C
C     ******************************************************************
C     ******************************************************************


C     ##################################################################
C
C     CHAMADA DA ROTINA DE IMPRESSAO DOS DADOS REFERENTES AO ALGORITMO
C     SIMULATED ANNEALING
C
C     ##################################################################


      OPEN(1,FILE=ARQ4,STATUS='UNKNOWN') 
C     OPEN(66,FILE='GIBBS.DAT',STATUS='UNKNOWN')

      WRITE(1,200)N,MAX,T,RT,EPS,NS,NT,NEPS,MAXEVL,IPRINT,ISEED1,ISEED2

 200  FORMAT(/,'SIMULATED ANNEALIG',/,/,'NUMERO DE PARAMETROS : ',I3,/,
     +       'MAXIMIZACAO : ',L5,/,'TEMPERATURA INICIAL : ',G8.2,/,
     +       'FATOR DE REDUCAO DE TEMPERATURA : ',G8.2,/,
     +       'ERRO MINIMO PARA TERMINO : ',G8.2,/,'NUMERO DE CICLOS : ',
     +       I3,/,'NUMERO DE ITERACOES ANTES DA REDUCAO DA TEMPERATURA:'
     +       ,I3,/,'NUMERO FINAL DO VALOR DA FUNCAO (NEPS) : ',I2,/,
     +       'NUMERO MAXIMO DE AVALIACOES DA FUNCAO : ',I10,/,
     +       'IPRINT : ',I1,/,'ISEED1 : ',I4,/,'ISEED2 : ',I4)
      
     

      CALL PRTVEC(X,N,'VALORES INICIAIS')
      CALL PRTVEC(VM,N,'VETOR PASSO INICIAL')
      CALL PRTVEC(LB,N,'LIMITE INFERIOR')
      CALL PRTVEC(UB,N,'LIMITE SUPERIOR')
      CALL PRTVEC(C,N,'VETOR C')
      
      WRITE(1,*)' '
      WRITE(1,*)'******************************************************'
      WRITE(1,*)'******************************************************'
      WRITE(1,*)' '            
      WRITE(1,*)'    FINAL DOS DADOS DE ENTRADA REFERENTES AO S.A.     '
      WRITE(1,*)' '
      WRITE(1,*)'******************************************************'
      WRITE(1,*)'******************************************************'
      WRITE(1,*)' '               

      IJI = 0

      CALL SA(N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,ISEED1,
     +        ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,FSTAR,XP,
     +        NACP,ATIVP)

      WRITE(10,*)' '
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)' '
      
      DO 300 I = 1,N
         WRITE(10,350)I,XOPT(I)
 300  CONTINUE
 350  FORMAT('XOPT(',I2,') : ',D23.10)
 
      FDELTA = FOPT - GZERO

      WRITE(*,*)'FOPT : ',FOPT
      WRITE(*,*)'FDELTA : ',FDELTA
      
      WRITE(10,*)'FOPT : ',FOPT
      WRITE(10,*)'FDELTA : ',FDELTA
      WRITE(10,*)'ATIV_ESTAB(1) : ',ATIVP(1)
      WRITE(10,*)' '
      WRITE(10,*)'O TESTE DE ESTABILIDADE FOI FEITO'
      WRITE(10,*)' '
      
      WRITE(10,*)' '
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)' '
      

 33   CONTINUE

      
C     ******************************************************************
C     ******************************************************************
C
C                   PROBLEMA DO EQUILIBRIO LIQUIDO-VAPOR      
C                        (SEGUNDA CHAMADA DO S.A.)
C
C     ******************************************************************
C     ******************************************************************


C     ------------------------------------------------------------------
C
C     PARA 5 E 8 COMPONENTES : FDELTA=-5.0D-9
C     PARA 10 COMPONENTES : FDELTA=-5.0D-06
C     PARA 3 COMPONENTES : FDELTA=-1.0D-05
C     PARA 2 COMPONENTES : FDELTA=-1.0D-05
C
C     ------------------------------------------------------------------
C
C     IF(FDELTA.LT.-5.0D-1)THEN
C     IF(FDELTA.LT.-1.0D-4)THEN
C     IF(FDELTA.LT.-5.0D-6)THEN
 
C     FDELTA = -1.1D-5

      IF(FDELTA.LT.-1.0D-5)THEN

C     IF(FDELTA.LT.-5.0D-9)THEN    
C     IF(FDELTA.LT.5.0D-4)THEN
C     IF(FDELTA.LT.-1.0D-7)THEN
C     IF(FDELTA.LT.-1.0D-10)THEN
      
      IJI=1
      
      WRITE(10,*)' '
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)' '      
      WRITE(10,*)'       PROBLEMA DO EQUILIBRIO LIQUIDO-VAPOR'
      WRITE(10,*)' '
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)'*****************************************************'
      WRITE(10,*)' ' 
      WRITE(10,*)' '      
      
      CALL SA(N,X,MAX,RT1,EPS,NS1,NT1,NEPS,MAXEVL1,LB,UB,C1,IPRINT,
     +        ISEED1,ISEED2,T1,VM1,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     +        FSTAR,XP,NACP,ATIVP)


           

C     ==================================================================
C
C     AVALIACAO DOS RESULTADOS NA SAIDA DO ELV.  IDENTIFICACAO DA FASE 
C     DE SAIDA E DAS FRACOES MASSICAS DOS COMPONENTES.
C
C     ==================================================================

      YSOMA = 0.0D0
      
      DO 600 I = 1,N-1
         YSOMA = XOPT(I) + YSOMA
 600  CONTINUE
      XMET = 1.0D0 - YSOMA         
      
      WRITE(10,*)'XOPT(1) : ',XOPT(1)
      WRITE(10,650)XMET
 650  FORMAT(/,'XOPT(2) : ',D23.10)
      WRITE(10,*)'FOPT : ',FOPT
      WRITE(10,*)'ATIVP(1) : ',ATIVP(1)
 
      IF(XMET.GT.Ze(N))THEN
      	WRITE(10,*)'RESULTADOS : XL(I) I = 1,N-1 ; L ; FASE LIQUIDA'
      	IF(XOPT(N).LT.0.50D0)THEN
      	  WRITE(10,*)'A FASE VAPOR E DOMINANTE'
      	  ELE = XOPT(N)
      	  V = 1.0D0 - ELE
      	  WRITE(10,*)'FASE VAPOR : ',V
      	  WRITE(10,*)'FASE LIQUIDA : ',ELE
      	ELSE
      	  WRITE(10,*)'FASE LIQUIDA DOMINANTE'
      	  ELE = XOPT(N)
      	  V = 1.0D0 - ELE
      	  WRITE(10,*)'FASE VAPOR : ',V
      	  WRITE(10,*)'FASE LIQUIDA : ',ELE
      	ENDIF
      	                   	       		   	        		
      
C     ==================================================================
C
C                RESULTADOS EM FUNCAO DA FASE VAPOR 
C
C     ==================================================================

	WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
	WRITE(10,*)' '
	WRITE(10,*)'RESULTADOS EM FUNCAO DA FASE VAPOR (V,XV)'
	WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
	WRITE(10,*)' '
	
	XVOPT(N) = 1.0D0 - XOPT(N)
	WRITE(10,700)XVOPT(N)
 700	FORMAT(/,'V : ',D23.10)
 
        DO 800 I = 1,N-1
           XVOPT(I) = (Ze(I) - XOPT(I)*XOPT(N))/XVOPT(N)
           WRITE(10,750)I,XVOPT(I)
 750       FORMAT(/,'XVOPT(',I2,') : ',D23.10)
 800    CONTINUE

        XVSOMA = 0.0D0

        DO 850 I = 1,N-1
           XVSOMA = XVSOMA + XVOPT(I)
 850    CONTINUE
        XMETV = 1.0D0 - XVSOMA
        WRITE(10,900)XMETV
 900     FORMAT(/,'XVOPT(2) : ',D23.10)

C     IF(MOD(IAVAL,13).EQ.0)THEN
        WRITE(57,*)'VAPOR'
        WRITE(57,1355)Pe,ATIVP(1)
 920    FORMAT(F23.10,3X,D23.18)
        WRITE(57,*)' '
C     ENDIF

        DO 930 I = 1,N-1
          XFV(I) = XVOPT(I)
 930    CONTINUE
        XFV(N) = XMETV

       
       CALL CONVW(N,XFV,COMP)

       WRITE(10,*)' '
       WRITE(10,*)'FRACAO EM MASSA DO SOLVENTE : ',Ze(1)
       WRITE(10,*)'FRACAO MOLAR DO SOLV. VAPOR : ',COMP(1)
       WRITE(10,*)'FRACAO MASSICA DO SOLV. VAPOR : ',XFV(1)         
            
C     ==================================================================
C
C     CALCULO DO PESO MOLECULAR MEDIO PARA IDENTIFICACAO OU CORRECAO DAS
C     FASES PRESENTES NA MISTURA.
C 
C     ==================================================================

        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        WRITE(10,*)'    VERIFICACAO E CORRECAO DAS FASES PRESENTES '
        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        
        
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
        
        WRITE(10,960)MASSALT
        WRITE(10,970)MASSAVT
 960    FORMAT(/,'MASSALT : ',D23.10)        	   
 970    FORMAT(/,'MASSAVT : ',D23.10,/)
 
        IF(MASSALT.GT.MASSAVT)THEN
          WRITE(10,*)'AS FASES ESTAO CORRETAS !!!!!! '
        ELSE
          WRITE(10,*)'FASES TROCADAS !!! APOS A CORRECAO, TEM-SE :'
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
             WRITE(10,990)I,XLOPT(I)
 990         FORMAT('XLOPT(',I2,') :',D23.10)
 1000     CONTINUE     
  
          WRITE(10,1050)V,ELE,XVAPI,XMETL
 1050     FORMAT(/,'V : ',D23.10,/,'L : ',D23.10,/,'XVAPI : ',D23.10,
     +           /,'XMETL : ',D23.10)
      	ENDIF
 

C     ==================================================================
C
C                   CALCULO DA CONSTANTE DE EQUILIBRIO     	      
C
C     ==================================================================

 
        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        WRITE(10,*)'    CALCULO DA CONSTANTE DE EQUILIBRIO (Ki) '
        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        WRITE(10,*)' '
        
        DO 1150 I = 1,N-1
           CA(I) = XVOPT(I)/XOPT(I)
           WRITE(10,1100)I,CA(I)
 1100      FORMAT('K(',I2,') : ',D23.10)
 1150   CONTINUE
        CA(N) = XMETV/XMET
        WRITE(10,1200)CA(N)
 1200   FORMAT('K(N) : ',D23.10,/)
 
      ELSE
 
      	WRITE(10,*)'RESULTADOS : XV(I) I = 1,N-1 ; V ; FASE VAPOR'
      	IF(XOPT(N).LT.0.5D0)THEN
      	  WRITE(10,*)'FASE LIQUIDA DOMINANTE !!!!'
      	  V = XOPT(N)
      	  ELE = 1.0D0 - V
      	  WRITE(10,*)' '
      	  WRITE(10,*)'V : ',V
      	  WRITE(10,*)'L : ',ELE
      	ELSE
      	  WRITE(10,*)'FASE VAPOR DOMINANTE !!!!' 
      	  V = XOPT(N)
      	  ELE = 1.0D0 - V
      	  WRITE(10,*)' '
      	  WRITE(10,*)'V : ',V
      	  WRITE(10,*)'L : ',ELE
      	ENDIF               
      	
C     ==================================================================
C
C                  RESULTADOS EM FUNCAO DA FASE LIQUIDA 
C
C     ==================================================================

	WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
	WRITE(10,*)' '
	WRITE(10,*)'    RESULTADOS EM FUNCAO DA FASE LIQUIDA (L,XL)'
	WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
	WRITE(10,*)' '
	WRITE(10,*)' '
      	
        XLOPT(N) = 1.0D0 - XOPT(N)
        WRITE(10,1250)XLOPT(N)
 1250   FORMAT(/,'L : ',D23.10,/)
 
        DO 1350 I = 1,N-1
           XLOPT(I) = (Ze(I) - XOPT(I)*XOPT(N))/XLOPT(N)
           WRITE(10,1300)I,XLOPT(I)
 1300      FORMAT('XLOPT(',I2,') : ',D23.10)
 1350   CONTINUE

C     IF(MOD(IAVAL,13).EQ.0)THEN
        WRITE(57,1355)Pe,ATIVP(1)
 1355   FORMAT(F23.10,3X,D23.18)
C     ENDIF

 
 	XLSOMA = 0.0D0
 	
 	DO 1400 I = 1,N-1
 	   XLSOMA = XLOPT(I) + XLSOMA
 1400   CONTINUE
 
 	XMETL = 1.0D0 - XLSOMA
 	WRITE(10,1450)XMETL
 1450 	FORMAT(/,'XLOPT(2) : ',D23.10) 

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

       WRITE(10,*)' '
       WRITE(10,*)'FRACAO EM MASSA DO SOLVENTE NA ALIM. : ',Ze(1)
       WRITE(10,*)'FRACAO MOLAR DO SOLV. LIQUIDO : ',COMP(1)
       WRITE(10,*)'FRACAO MASSICA DO SOLV. LIQUIDO : ',XFL(1)       
        
C     ==================================================================
C
C     CALCULO DO PESO MOLECULAR MEDIO PARA IDENTIFICACAO OU CORRECAO DAS
C     FASES PRESENTES NA MISTURA.
C 
C     ==================================================================

        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        WRITE(10,*)'    VERIFICACAO E CORRECAO DAS FASES PRESENTES '
        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        
        
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
        
        WRITE(10,1550)MASSALT
        WRITE(10,1600)MASSAVT
 1550   FORMAT(/,'MASSALT : ',D23.10)        	   
 1600   FORMAT(/,'MASSAVT : ',D23.10,/)
 
        IF(MASSALT.GT.MASSAVT)THEN
          WRITE(10,*)'AS FASES ESTAO CORRETAS !!!!!! '
        ELSE
          WRITE(10,*)'FASES TROCADAS !!! APOS A CORRECAO, TEM-SE :'
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
             WRITE(10,1650)I,XVOPT(I)
 1650        FORMAT('XVOPT(',I2,') :',D23.10)
 1700     CONTINUE     
  
          WRITE(10,1750)V,ELE,XLIQU,XMETV
 1750     FORMAT(/,'V : ',D23.10,/,'L : ',D23.10,/,'XLIQU : ',D23.10,
     +           /,'XMETV : ',D23.10)
      	ENDIF
      	
      	
C     ==================================================================
C
C                   CALCULO DA CONSTANTE DE EQUILIBRIO     	      
C
C     ==================================================================

 
        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        WRITE(10,*)'    CALCULO DA CONSTANTE DE EQUILIBRIO (Ki) '
        WRITE(10,*)' '
        WRITE(10,*)'***************************************************'
        WRITE(10,*)' '
        WRITE(10,*)' '
        
        DO 1850 I = 1,N-1
           CA(I) = XOPT(I)/XLOPT(I)
           WRITE(10,1800)I,CA(I)
 1800      FORMAT('K(',I2,') : ',D23.10)
 1850   CONTINUE
        CA(N) = XMET/XMETL
        WRITE(10,1900)CA(N)
 1900   FORMAT('K(N) : ',D23.10,/)
 
      ENDIF
      

C     ESTE 'ELSE' RELACIONA-SE AO IF DO NUMERO DE COMPONENTES      

      ELSE

C     ==================================================================
C
C     UMA UNICA FASE (DETERMINACAO DA FASE PREDOMINANTE) E TESTE PARA 
C     ALFAL E ALFAV.
C
C     ==================================================================


	WRITE(10,*)' '
	WRITE(10,*)'           EXISTE APENAS UMA UNICA FASE '
	WRITE(10,*)' '
	WRITE(10,*)' '
	
	
	IF((XOPT(1).LT.Ze(1)).OR.(ALFAL.GT.ALFAV))THEN
	  WRITE(10,*)'RESULTADOS : XL(I) I = 1,N-1 ; L ; FASE LIQUIDA'
      	  IF(XOPT(N).LT.0.5D0)THEN
      	    WRITE(10,*)'FASE VAPOR DOMINANTE !!!!'
      	    V = 1.0D0
      	    ELE = 0.0D0
      	    WRITE(10,*)' '
      	    WRITE(10,*)'V : ',V
      	    WRITE(10,*)'L : ',ELE
      	  ELSE
      	    WRITE(10,*)'FASE LIQUIDA DOMINANTE !!!!' 
      	    V = 0.0D0
      	    ELE = 1.0D0
      	    WRITE(10,*)' '
      	    WRITE(10,*)'V : ',V
      	    WRITE(10,*)'L : ',ELE
      	  ENDIF               
      	ELSE
      	  WRITE(10,*)'RESULTADOS : XV(I) I = 1,N-1 ; V ; FASE VAPOR'
      	  IF(XOPT(N).LT.0.5D0)THEN
      	    WRITE(10,*)'FASE LIQUIDA DOMINANTE !!!!'
      	    ELE = 1.0D0
      	    V = 0.0D0
      	    WRITE(10,*)' '
      	    WRITE(10,*)'V : ',V
      	    WRITE(10,*)'L : ',ELE
      	  ELSE
      	    WRITE(10,*)'FASE VAPOR DOMINANTE !!!!' 
      	    ELE = 0.0D0
      	    V = 1.0D0
      	    WRITE(10,*)' '
      	    WRITE(10,*)'V : ',V
      	    WRITE(10,*)'L : ',ELE
      	  ENDIF   
      	ENDIF

      ENDIF    

     
C     ==================================================================
C
C                  IMPRESSOES DOS RESULTADOS FINAIS DO S.A.            
C
C     ==================================================================


      WRITE(1,*)' '
      WRITE(1,*)' '
      WRITE(1,*)'******************************************************'            
      WRITE(1,*)'******************************************************'            
      WRITE(1,*)' '      
      WRITE(1,*)' '
      WRITE(1,*)'       RESULTADOS FINAIS APOS A IMPLEMENTACAO DO S.A.'   
      WRITE(1,*)' '
      WRITE(1,*)' '
      WRITE(1,*)'******************************************************'            
      WRITE(1,*)'******************************************************'            
      WRITE(1,*)' '      
      WRITE(1,*)' '
      
      CALL PRTVEC(XOPT,N,'SOLUCAO FINAL')
      CALL PRTVEC(VM,N,'VETOR VM FINAL')      
      
      WRITE(1,*)' '
      WRITE(1,2000)FOPT,NFCNEV,NACC,NOBDS,T,IER
 2000 FORMAT(/,'VALOR OTIMO DA FUNCAO : ',D20.13,/,
     +       'NUMERO DE AVALIACOES DA FUNCAO : ',I10,/,
     +       'NUMERO DE AVALIACOES ACEITAS : ',I10,/,
     +       'NUMERO DE AVALIACOES FORA DOS LIMITES : ',I10,/,
     +       'PARAMETRO TEMPERATURA - FINAL : ',G20.13,/,'IER : ',I3)
     
C     STOP

C     ===============================================================
C     ***************************************************************
C     ===============================================================
C
C     			FIM DO LOOP
C
C     ===============================================================
C     ***************************************************************
C     ===============================================================

      
      WRITE(*,*)'Pe : ',Pe

      IF(IOPC.EQ.1)THEN
        IF(Pe.LT.Pf)THEN
          Pe = Pe + Pd
          IAVAL = IAVAL + 1
          GOTO 7777
        ENDIF
      ENDIF

      STOP
      END
