
C     ARQUIVO SA.F

      SUBROUTINE SA(N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,
     +              ISEED1,ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     +              FSTAR,XP,NACP,ATVOP)


C     ******************************************************************
C     ******************************************************************
C
C     Version: 3.2
C     Date: 1/22/94.
C     Differences compared to Version 2.0:
C     1. If a trial is out of bounds, a point is randomly selected
C        from LB(i) to UB(i). Unlike in version 2.0, this trial is
C        evaluated and is counted in acceptances and rejections.
C        All corresponding documentation was changed as well.
C     Differences compared to Version 3.0:
C     1. If VM(i) > (UB(i) - LB(i)), VM is set to UB(i) - LB(i).
C        The idea is that if T is high relative to LB & UB, most
C        points will be accepted, causing VM to rise. But, in this
C        situation, VM has little meaning; particularly if VM is
C        larger than the acceptable region. Setting VM to this size
C        still allows all parts of the allowable region to be selected.
C     Differences compared to Version 3.1:
C     	1. Test made to see if the initial temperature is positive.
C     	2. WRITE statements prettied up.
C     	3. References to paper updated.
C
C     Synopsis:
C     This routine implements the continuous simulated annealing global
C     optimization algorithm described in Corana et al.'s article
C     "Minimizing Multimodal Functions of Continuous Variables with the
C     "Simulated Annealing" Algorithm" in the September 1987 (vol. 13,
C     no. 3, pp. 262-280) issue of the ACM Transactions on Mathematical
C     Software.
C
C     A very quick (perhaps too quick) overview of SA:
C     SA tries to find the global optimum of an N dimensional function.
C     It moves both up and downhill and as the optimization process
C     proceeds, it focuses on the most promising area.
C     To start, it randomly chooses a trial point within the step length
C     VM (a vector of length N) of the user selected starting point. The
C     function is evaluated at this trial point and its value is 
C     compared to its value at the initial point.
C     In a maximization problem, all uphill moves are accepted and the
C     algorithm continues from that trial point. Downhill moves may be
C     accepted; the decision is made by the Metropolis criteria. It uses
C     T(temperature) and the size of the downhill move in a 
C     probabilistic manner. The smaller T and the size of the downhill 
C     move are, the morelikely that move will be accepted. If the trial 
C     is accepted, the algorithm moves on from that point. If it is 
C     rejected, another point is chosen instead for a trial evaluation.
C     Each element of VM periodically adjusted so that half of all
C     function evaluations in that direction are accepted.
C     A fall in T is imposed upon the system with the RT variable by
C     T(i+1) = RT*T(i) where i is the ith iteration. Thus, as T 
C     declines,downhill moves are less likely to be accepted and the 
C     percentage of rejections rise. Given the scheme for the selection 
C     for VM, VM falls.Thus, as T declines, VM falls and SA focuses upon
C     the most promising area for optimization.
C
C     The importance of the parameter T:
C     The parameter T is crucial in using SA successfully. It influences
C     VM, the step length over which the algorithm searches for optima. 
C     For a small intial T, the step length may be too small; thus not 
C     enough of the function might be evaluated to find the global 
C     optima. The user should carefully examine VM in the intermediate 
C     output (set IPRINT = 1) to make sure that VM is appropriate. The 
C     relationship between the initial temperature and the resulting 
C     step length is function dependent.
C
C     To determine the starting temperature that is consistent with
C     optimizing a function, it is worthwhile to run a trial run first. 
C     Set RT = 1.5 and T = 1.0. With RT > 1.0, the temperature increases
C     and VM rises as well. Then select the T that produces a large 
C     enough VM.
C
C     For modifications to the algorithm and many details on its use,
C     (particularly for econometric applications) see Goffe, Ferrier
C     and Rogers, "Global Optimization of Statistical Functions with
C     Simulated Annealing," Journal of Econometrics, vol. 60, no. 1/2, 
C     Jan./Feb. 1994, pp. 65-100.
C     For more information, contact 
C              Bill Goffe
C              Department of Economics and International Business
C              University of Southern Mississippi 
C              Hattiesburg, MS  39506-5072 
C              (601) 266-4484 (office)
C              (601) 266-4920 (fax)
C              bgoffe@whale.st.usm.edu (Internet)
C
C     As far as possible, the parameters here have the same name as in
C     the description of the algorithm on pp. 266-8 of Corana et al.
C
C     In this description, SP is single precision, DP is double 
C     precision,INT is integer, L is logical and (N) denotes an array of
C     length n. Thus, DP(N) denotes a double precision array of length 
C     n.
C
C     Input Parameters:
C     Note: The suggested values generally come from Corana et al. To
C          drastically reduce runtime, see Goffe et al., pp. 90-1 for
C          suggestions on choosing the appropriate RT and NT.

C     N - Number of variables in the function to be optimized. (INT)
C     X - The starting values for the variables of the function to be
C        optimized. (DP(N))
C     MAX - Denotes whether the function should be maximized or
C          minimized. A true value denotes maximization while a false
C          value denotes minimization. Intermediate output (see IPRINT)
C          takes this into account. (L)
C     RT - The temperature reduction factor. The value suggested by
C         Corana et al. is .85. See Goffe et al. for more advice. (DP)
C     EPS - Error tolerance for termination. If the final function
C          values from the last neps temperatures differ from the
C          corresponding value at the current temperature by less than
C          EPS and the final function value at the current temperature
C          differs from the current optimal function value by less than
C          EPS, execution terminates and IER = 0 is returned. (EP)
C     NS - Number of cycles. After NS*N function evaluations, each
C         element of VM is adjusted so that approximately half of
C         all function evaluations are accepted. The suggested value
C         is 20. (INT)
C     NT - Number of iterations before temperature reduction. After
C         NT*NS*N function evaluations, temperature (T) is changed
C         by the factor RT. Value suggested by Corana et al. is
C         MAX(100, 5*N). See Goffe et al. for further advice. (INT)
C     NEPS - Number of final function values used to decide upon termi-
C           nation. See EPS. Suggested value is 4. (INT)
C     MAXEVL - The maximum number of function evaluations. If it is
C             exceeded, IER = 1. (INT)
C     LB - The lower bound for the allowable solution variables. (DP(N))
C     UB - The upper bound for the allowable solution variables. (DP(N))
C         If the algorithm chooses X(I) .LT. LB(I) or X(I) .GT. UB(I),
C         I = 1, N, a point is from inside is randomly selected. This
C         This focuses the algorithm on the region inside UB and LB.
C         Unless the user wishes to concentrate the search to a par-
C         ticular region, UB and LB should be set to very large positive
C         and negative values, respectively. Note that the starting
C         vector X should be inside this region. Also note that LB and
C         UB are fixed in position, while VM is centered on the last
C         accepted trial set of variables that optimizes the function.
C     C - Vector that controls the step length adjustment. The suggested
C        value for all elements is 2.0. (DP(N))
C     IPRINT - controls printing inside SA. (INT)
C             Values: 0 - Nothing printed.
C                     1 - Function value for the starting value and
C                         summary results before each temperature
C                         reduction. This includes the optimal
C                         function value found so far, the total
C                         number of moves (broken up into uphill,
C                         downhill, accepted and rejected), the
C                         number of out of bounds trials, the
C                         number of new optima found at this
C                         temperature, the current optimal X and
C                         the step length VM. Note that there are
C                         N*NS*NT function evalutations before each
C                         temperature reduction. Finally, notice is
C                         is also given upon achieveing the termination
C                         criteria.
C                     2 - Each new step length (VM), the current optimal
C                         X (XOPT) and the current trial X (X). This
C                         gives the user some idea about how far X
C                         strays from XOPT as well as how VM is adapting
C                         to the function.
C                     3 - Each function evaluation, its acceptance or
C                         rejection and new optima. For many problems,
C                         this option will likely require a small tree
C                         if hard copy is used. This option is best
C                         used to learn about the algorithm. A small
C                         value for MAXEVL is thus recommended when
C                         using IPRINT = 3.
C             Suggested value: 1
C             Note: For a given value of IPRINT, the lower valued
C                   options (other than 0) are utilized.
C     ISEED1 - The first seed for the random number generator RANMAR.
C             0 .LE. ISEED1 .LE. 31328. (INT)
C     ISEED2 - The second seed for the random number generator RANMAR.
C             0 .LE. ISEED2 .LE. 30081. Different values for ISEED1
C             and ISEED2 will lead to an entirely different sequence
C             of trial points and decisions on downhill moves (when
C             maximizing). See Goffe et al. on how this can be used
C             to test the results of SA. (INT)
C
C     Input/Output Parameters:
C     T - On input, the initial temperature. See Goffe et al. for advice.
C        On output, the final temperature. (DP)
C     VM - The step length vector. On input it should encompass the
C         region of interest given the starting value X. For point
C         X(I), the next trial point is selected is from X(I) - VM(I)
C         to  X(I) + VM(I). Since VM is adjusted so that about half
C         of all points are accepted, the input value is not very
C         important (i.e. is the value is off, SA adjusts VM to the
C         correct value). (DP(N))
C
C     Output Parameters:
C     XOPT - The variables that optimize the function. (DP(N))
C     FOPT - The optimal value of the function. (DP)
C     NACC - The number of accepted function evaluations. (INT)
C     NFCNEV - The total number of function evaluations. In a minor
C             point, note that the first evaluation is not used in the
C             core of the algorithm; it simply initializes the
C             algorithm. (INT).
C     NOBDS - The total number of trial function evaluations that
C            would have been out of bounds of LB and UB. Note that
C            a trial point is randomly selected between LB and UB.
C            (INT)
C     IER - The error return number. (INT)
C          Values: 0 - Normal return; termination criteria achieved.
C                  1 - Number of function evaluations (NFCNEV) is
C                      greater than the maximum number (MAXEVL).
C                  2 - The starting value (X) is not inside the
C                      bounds (LB and UB).
C                  3 - The initial temperature is not positive.
C                  99 - Should not be seen; only used internally.
C
C     Work arrays that must be dimensioned in the calling routine:
C       RWK1 (DP(NEPS))  (FSTAR in SA)
C       RWK2 (DP(N))     (XP    "  " )
C       IWK  (INT(N))    (NACP  "  " )
C
C     Required Functions (included):
C     EXPREP - Replaces the function EXP to avoid under- and overflows.
C             It may have to be modified for non IBM-type main-
C             frames. (DP)
C     RMARIN - Initializes the random number generator RANMAR.
C     RANMAR - The actual random number generator. Note that
C             RMARIN must run first (SA does this). It produces uniform
C             random numbers on [0,1]. These routines are from
C             Usenet's comp.lang.fortran. For a reference, see
C             "Toward a Universal Random Number Generator"
C             by George Marsaglia and Arif Zaman, Florida State
C             University Report: FSU-SCRI-87-50 (1987).
C             It was later modified by F. James and published in
C             "A Review of Pseudo-random Number Generators." For
C             further information, contact stuart@ads.com. These
C             routines are designed to be portable on any machine
C             with a 24-bit or more mantissa. I have found it produces
C             identical results on a IBM 3081 and a Cray Y-MP.
C
C     Required Subroutines (included):
C     PRTVEC - Prints vectors.
C     PRT1 ... PRT10 - Prints intermediate output.
C     FCN - Function to be optimized. The form is
C            SUBROUTINE FCN(N,X,F)
C            INTEGER N
C            DOUBLE PRECISION  X(N), F
C            ...
C            function code with F = F(X)
C            ...
C            RETURN
C            END
C          Note: This is the same form used in the multivariable
C          minimization algorithms in the IMSL edition 10 library.
C
C     Machine Specific Features:
C     1. EXPREP may have to be modified if used on non-IBM type main-
C       frames. Watch for under- and overflows in EXPREP.
C     2. Some FORMAT statements use G25.18; this may be excessive for
C       some machines.
C     3. RMARIN and RANMAR are designed to be protable; they should not
C       cause any problems.

C     Type all external variables.
C
C     ******************************************************************
C     ******************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'

      DOUBLE PRECISION  X(*),LB(*),UB(*),C(*),VM(*),FSTAR(*),XOPT(*), 
     +                  XP(*),T,EPS,RT,FOPT,F,FP,P,PP,RATIO,EXPREP
      DOUBLE PRECISION ATIVL(2),ATVOP(2),ATIVP(2)
      DOUBLE PRECISION CPM(2),GG(1000)
      REAL RANMAR
      INTEGER  NACP(*),N,NS,NT,NEPS,NACC,MAXEVL,IPRINT,NOBDS,IER,NFCNEV,
     +         ISEED1,ISEED2,NUP,NDOWN,NREJ,NNEW,LNOBDS,H,I,J,M 
      LOGICAL  MAX,QUIT


C     ##################################################################
C
C     CHAMADA PRELIMINAR DA SUBROTINA DE GERACAO DE NUMEROS RANDOMICOS
C
C     ##################################################################

      CALL RMARIN(ISEED1,ISEED2)
      
      
C     ==================================================================
C
C     			 INICIALIZACAO DOS PARAMETROS 
C
C     ==================================================================
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      NACC = 0
      NOBDS = 0
      NFCNEV = 0
      IER = 99

      DO 10, I = 1, N
         XOPT(I) = X(I)
         NACP(I) = 0
10    CONTINUE

      DO 20, I = 1, NEPS
         FSTAR(I) = 1.0D+20
20    CONTINUE 


C     ==================================================================
C
C     SE A TEMPERATURA INICIAL FOR NAO POSITIVA, AVISAR E RETORNAR AO
C     PROGRAMA PRINCIPAL.
C
C     ==================================================================


      IF(T.LE.0.0D0)THEN
      	WRITE(1,*)'A TEMPERATURA INICIAL POSSUI VALOR NAO POSITIVO !!!'
      	WRITE(1,*)'MODIFIQUE-A !!!!'
      	IER = 3
      	RETURN
      ENDIF
	
C     ==================================================================
C
C     SE O VALOR DAS COMPOSICOES ESTIVER FORA DOS LIMITES, AVISAR E RE -
C     TORNAR AO PROGRAMA PRINCIPAL.	
C
C     ==================================================================

      DO 30 I = 1, N
         IF((X(I) .GT. UB(I)).OR.(X(I).LT.LB(I)))THEN
      		CALL PRT1
            	IER = 2
            	RETURN
         END IF	
30    CONTINUE


C     ==================================================================
C     ==================================================================
C
C     		AVALIACAO DA FUNCAO (ESTIMATIVA INICIAL)
C
C     ==================================================================
C     ==================================================================


      CALL FCN(N,X,F,ATIVL)

      IF(IJI.EQ.1)THEN
        ITGG=1
        CALL GIBBS(N,X,F,ITGG)
      ENDIF


      
C     ------------------------------------------------------------------
C
C     SE A FUNCAO FOR MINIMIZADA, TROCAR OS SINAIS. TODAS AS SAIDAS  FI-
C     NAIS E INTERMEDIARIAS TROCAM OS SINAIS PARA EVITAR POSSIVEIS CON-
C     FUSOES.
C
C     ------------------------------------------------------------------

      IF(.NOT.MAX)F = -F
      NFCNEV = NFCNEV + 1
      FOPT = F
      FSTAR(1) = F
      DO 35 I = 1,N
        ATVOP(I) = ATIVL(I)
 35   CONTINUE
      IF(IPRINT.GE.1)CALL PRT2(MAX,N,X,F)
      

C     ==================================================================
C
C     				INICIO DO LOOP
C
C     ==================================================================


C     ------------------------------------------------------------------
C
C     O LOOP TERMINA QUANDO :
C     (1) O ALGORITMO S.A. OTIMIZA A FUNCAO COM SUCESSO ;
C     (2) HA MUITAS AVALIACOES DA FUNCAO (DETERMINADA POR MAXEVL)
C
C     ------------------------------------------------------------------

      NF = 0
      NF = MAXEVL
      
      DO 5000 KLOOP = 1,NF
      
      	NUP = 0
      	NREJ = 0
      	NNEW = 0
      	NDOWN = 0
      	LNOBDS = 0
	      	
      	DO 400 M = 1,NT
      	   DO 300 J = 1,NS
      	      DO 200 H = 1,N
      	         
 7000 CONTINUE      	               	      
C     ------------------------------------------------------------------
C
C           GERACAO DO VETOR XP, A PARTIR DOS VETORES X E VM
C
C     ------------------------------------------------------------------               		
   		 DO 110 I = 1,N
   		    IF(I.EQ.H)THEN
   		    	XP(I) = X(I) + (RANMAR()*2.0 - 1.0)*VM(I)
   		    ELSE
   		    	XP(I) = X(I)
   		    ENDIF
   		    
C     ------------------------------------------------------------------
C
C     SE O VETOR XP ESTIBER FORA DOS LIMITES, SELECIONAR UM DOS EXTREMOS
C     COMO TENTATIVA PRELIMINAR
C
C     ------------------------------------------------------------------

   		    
                    IF((XP(I).LT.LB(I)).OR.(XP(I).GT.UB(I)))THEN
                    	XP(I) = LB(I) + (UB(I) - LB(I))*RANMAR()
                    	LNOBDS = LNOBDS + 1
                    	NOBDS = NOBDS + 1
                    	IF(IPRINT.GE.3)CALL PRT3(MAX,N,XP,X,FP,F)
                    ENDIF
 110              CONTINUE
 

C     ==================================================================
C
C                ALTERACAO PARA GARANTIA DE FACTIBILIDADE
C
C     ==================================================================

                  SOMAXP = 0.0D0
                  SSOMA = 0.0D0
                  SMIN = Ze(1)/XP(1)
                  DO 115 I =1,N-1
                     ZXL1 = Ze(I)/XP(I)
                     SMIN = DMIN1(ZXL1,SMIN)
                     ZXL1 = (1.0D0 - Ze(I))/(1.0D0 - XP(I))
                     SMIN = DMIN1(ZXL1,SMIN)
                     SOMAXP = XP(I) + SOMAXP
                     ZXL = (Ze(I) - XP(I)*XP(N))/(1.0D0 - XP(N))
                     SSOMA = ZXL + SSOMA
 115     	  CONTINUE
                  
                  IF(XP(N).GE.SMIN)XP(N) = SMIN - 1.0D-03
                  	
                  IF(SOMAXP.GE.0.99990D0)GOTO 7000
                  
                  IF((SSOMA.GE.0.99990D0).OR.(SSOMA.LE.0.0D0))GOTO 7000
                  

C     ------------------------------------------------------------------
C
C     AVALIACAO DA FUNCAO COM OS VALORES DO VETOR XP, OBTENDO COMO RES-
C     POSTA FP
C
C     ------------------------------------------------------------------

		  DO 117 I = 1,N-1
		     ZXL = (Ze(I) - XP(I)*XP(N))/(1.0D0 - XP(N))
 117              CONTINUE
 

                  CALL FCN(N,XP,FP,ATIVP)

	          IF(IJI.EQ.1)THEN
                    ITGG=ITGG+1
                    CALL GIBBS(N,X,F,ITGG)
      		  ENDIF
   
                  IF(.NOT.MAX)FP = -FP
 
                  NFCNEV = NFCNEV + 1
                  IF(IPRINT.GE.3)CALL PRT4(MAX,N,XP,X,FP,F)
                  

C     ------------------------------------------------------------------
C
C     SE JA TIVEREM SIDO FEITAS MUITAS AVALIACOES DA FUNCAO, TERMINAR O
C     ALGORITMO S.A.
C
C     ------------------------------------------------------------------

                  IF(NFCNEV.GE.MAXEVL)THEN
                  	CALL PRT5
                  	IF(.NOT.MAX)FOPT = -FOPT
                  	IER = 1
                  	RETURN
                  ENDIF
                  
                  
C     ------------------------------------------------------------------
C
C            O NOVO PONTO E ACEITO SE O VALOR DA FUNCAO AUMENTA                  
C
C     ------------------------------------------------------------------

                  
                  IF(FP.GE.F)THEN
                  	IF(IPRINT.GE.3)THEN
                  	  WRITE(1,*)'NOVO PONTO ACEITO'
                  	ENDIF
                  	DO 120 I = 1,N                  
                       	   X(I) = XP(I)
                           ATIVL(I) = ATIVP(I)
 120                  	CONTINUE
                        F = FP
                        NACC = NACC + 1
                        NACP(H) = NACP(H) + 1
                        NUP = NUP + 1
                        
C     ------------------------------------------------------------------
C
C     SE O VALOR DA FUNCAO FOR MAIOR QUE QUALQUER OUTRO PONTO, ESTE SERA
C     O NOVO VALOR OTIMO DA FUNCAO.
C
C     ------------------------------------------------------------------

 			IF(FP.GT.FOPT)THEN
 			  IF(IPRINT.GE.3)THEN
 			    WRITE(1,*)'NOVO VALOR OTIMO DA FUNCAO'
 			  ENDIF                      
 			  DO 130 I = 1,N
 			     XOPT(I) = XP(I)
                             ATVOP(I) = ATIVP(I)
 130                      CONTINUE
                          FOPT = FP
                          NNEW = NNEW + 1
                        ENDIF
                        
                  
C     ------------------------------------------------------------------
C
C     SE O PONTO E MENOR QUE OUTROS, USAR O CRITERIO DE METROPOLIS PARA
C     DECIDIR SE ESTE E ACEITO OU REJEITADO.
C
C     ------------------------------------------------------------------

                  ELSE
                  	P = EXPREP((FP - F)/T)
                  	PP = RANMAR()
                  	IF(PP.LT.P)THEN
                  	  IF(IPRINT.GE.3)CALL PRT6(MAX)
                  	  DO 140 I = 1,N
                  	     X(I) = XP(I)
 140                      CONTINUE
                          F = FP
                          NACC = NACC + 1
                          NACP(H) = NACP(H) + 1
                          NDOWN = NDOWN + 1
                        ELSE
                          NREJ = NREJ + 1
                          IF(IPRINT.GE.3)CALL PRT7(MAX)
                        ENDIF
                  ENDIF
                  
 200	      CONTINUE
 300       CONTINUE
 

C     ------------------------------------------------------------------
C
C     AJUSTE DO VETOR VM DE MODO QUE APROXIMADAMENTE METADE DE TODAS AS
C     AVALIACOES SEJAM ACEITAS
C
C     ------------------------------------------------------------------

	   DO 310 I = 1,N
	      RATIO = DFLOAT(NACP(I))/DFLOAT(NS)
	      IF(RATIO.GT.0.6D0)THEN
	        VM(I) = VM(I)*(1.0D0 + C(I)*(RATIO - 0.6D0)/0.4D0)
	      ELSEIF(RATIO.LT.0.4D0)THEN
	        VM(I) = VM(I)*(1.0D0 + C(I)*(0.41D0 - RATIO)/0.4D0)
	      ENDIF
	      IF(VM(I).GT.(UB(I) - LB(I)))THEN
	        VM(I) = UB(I) - LB(I)
	      ENDIF
 310       CONTINUE
 
           IF(IPRINT.GE.2)THEN
             CALL PRT8(N,VM,XOPT,X)
           ENDIF
           
           DO 320 I = 1,N
              NACP(I) = 0
 320       CONTINUE
           
 400	CONTINUE
 
 	IF(IPRINT.GE.1)THEN
 	  CALL PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)
  	ENDIF     
 	

C     ------------------------------------------------------------------
C
C                   VERIFICACAO DO CRITERIO DE PARADA 	     
C
C     ------------------------------------------------------------------

      	QUIT = .FALSE.
      	FSTAR(1) = F
      	IF((FOPT - FSTAR(1)).LE.EPS)QUIT = .TRUE.
      	DO 410 I = 1,NEPS
      	   IF(ABS(F - FSTAR(I)).GT.EPS)QUIT = .FALSE.
 410    CONTINUE
 
 
C     ------------------------------------------------------------------
C
C                VERIFICACAO DO TERMINO DO ALGORITMO S.A. 
C
C     ------------------------------------------------------------------


	IF(QUIT)THEN
	  DO 420 I = 1,N
	     X(I) = XOPT(I)
             ATIVL(I) = ATVOP(I)
 420      CONTINUE
 	  IER = 0
 	  IF(.NOT.MAX)FOPT = -FOPT
 	  IF(IPRINT.GE.1)CALL PRT10
 	  RETURN
 	ENDIF
 	
 	
C     ------------------------------------------------------------------
C
C        SE O CRITERIO DE PARADA NAO E ALCANCADO, RETORNA-SE O LOOP. 	
C
C     ------------------------------------------------------------------

 	  
      	T = RT*T
      	DO 430 I = NEPS,2,-1
          FSTAR(I) = FSTAR(I-1)
 430    CONTINUE
        F = FOPT
        DO 440 I = 1,N
          X(I) = XOPT(I)
 440    CONTINUE

 5000 CONTINUE


      END
      
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================            


      FUNCTION  EXPREP(RDUM)
      
      
C     ------------------------------------------------------------------
C
C     ESTA FUNCAO TOMA LUGAR DA FUNCAO INTRINSECA 'EXP' DE MODO A EVITAR
C     (UNDER/OVER)FLOW E E ESPECIFICA PARA IBM 370.  
C
C     ------------------------------------------------------------------


      DOUBLE PRECISION RDUM,EXPREP

      IF(RDUM.GT.174.0D0)THEN
         EXPREP = 3.69D+75
      ELSEIF(RDUM.LT.-180.D0)THEN
         EXPREP = 0.0D0
      ELSE
         EXPREP = EXP(RDUM)
      END IF

      RETURN
      END
      
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================


      SUBROUTINE RMARIN(IJ,KL)
      
      
C     ------------------------------------------------------------------
C
C     ESTA SUBROTINA E A FUNCAO RANMAR GERAM NUMEROS RANDOMICOS USADOS
C     NO ALGORITMO S.A.  NA VERDADE ESTA SUBROTINA, INICIALIZA A FUNCAO
C     GERADORA RANMAR().  OS ARGUMENTOS DEVEM ESTAR NO LIMITE :
C     0 <= IJ <= 31328  E  0 <= KL <= 30081.
C
C     ------------------------------------------------------------------
                  
      INTEGER I97,J97
      REAL U(97),C,CD,CM
      
      COMMON/RASET1/U,C,CD,CM,I97,J97
      
      IF((IJ.LT.0).OR.(IJ.GT.31328).OR.(KL.LT.0).OR.(KL.GT.30081))THEN
      	WRITE(1,*)' '
      	WRITE(1,*)'O PRIMEIRO ALIMENTADOR DO NUMERO RANDOMICO (ISEED1)'
      	WRITE(1,*)'DEVE TER VALORES ENTRE 0 E 31328. VERIFIQUE !!!!'
   	WRITE(1,*)' '
      	WRITE(1,*)'O SEGUNDO ALIMENTADOR DO NUMERO RANDOMICO (ISEED2)'
      	WRITE(1,*)'DEVE TER VALORES ENTRE 0 E 30081. VERIFIQUE !!!!'
      	WRITE(1,*)' '
      	STOP
      ENDIF

      I = MOD(IJ/177,177) + 2
      J = MOD(IJ,177) + 2
      K = MOD(KL/169,178) + 1
      L = MOD(KL,169)
      
      DO 20 II = 1,97
      	 S = 0.0
      	 T = 0.5
      	 DO 10 JJ = 1,24
	    M = MOD((K*MOD(I*J,179)),179)
	    I = J
	    J = K
	    K = M
	    L = MOD((53*L + 1),169)
	    IF((MOD((L*M),64)).GE.32)THEN
	    	S = S + T
	    ENDIF
	    T = 0.5*T
 10   	 CONTINUE
 	 U(II) = S
 20   CONTINUE
 
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
      
      RETURN
      END	    


C     ==================================================================
C     ******************************************************************
C     ==================================================================

      FUNCTION RANMAR()
      
      REAL U(97),C,CD,CM
      INTEGER I97,J97
      
      COMMON/RASET1/U,C,CD,CM,I97,J97
      
      UNI = U(I97) - U(J97)
      
      IF(UNI.LT.0.0)UNI = UNI + 1.0
      U(I97) = UNI
      I97 = I97 - 1
      
      IF(I97.EQ.0)I97 = 97
      J97 = J97 - 1
      
      IF(J97.EQ.0)J97 = 97
      C = C - CD
      
      IF(C.LT.0.0)C = C + CM
      UNI = UNI - C
      
      IF(UNI.LT.0.0)UNI = UNI + 1.0
      RANMAR = UNI
      
      RETURN
      END
            
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================      

      SUBROUTINE PRT1
      

C     ESTA ROTINA (E AS PRT(N)) IMPRIME OS VALORE INTERMEDIARIOS DO S.A.
C     NOTE QUE SE S.A. ESTA MINIMIZANDO UMA FUNCAO, O SINAL DO VALOR DA
C     FUNCAO E AS DIRECOES (PARA CIMA E PARA BAIXO) ESTAO INVERTIDOS EM
C     TODAS AS SAIDAS.  ISTO EH DEVIDO AO S.A. TER SIDO ESCRITO ORIGINAL
C     MENTE PARA MAXIMIZAR FUNCOES, ASSIM ESTE MINIMIZA FUNCOES PELA MA-
C     MIZACAO DO NEGATIVO DESTAS.

      WRITE(1,*)'OS VALORES INICIAIS DE X(I) ESTAO FORA DOS LIMITES. O'
      WRITE(1,*)'ALGORITMO SERA SUSPENSO ATE A DEVIDA MODIFICACAO DOS'
      WRITE(1,*)'DO VETOR'
     
      RETURN
      END
      
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================
 
      
      SUBROUTINE PRT2(MAX,N,X,F)

      DOUBLE PRECISION X(*),F
      INTEGER N
      LOGICAL MAX
      
      WRITE(1,*)' ' 
      CALL PRTVEC(X,N,'VALORES INICIAIS DE X')
      IF(MAX)THEN
      	WRITE(1,10)F
 10     FORMAT(/,'VALOR INICIAL DE F :',D25.18)
      ELSE
      	WRITE(1,20)-1.0D0*F
 20     FORMAT(/,'VALOR INICIAL DE F :',D25.18) 
      ENDIF
      
      RETURN
      END
      
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================       
      

      SUBROUTINE PRT3(MAX,N,XP,X,FP,F)

      INTEGER  N
      DOUBLE PRECISION XP(*),X(*),FP,F
      LOGICAL  MAX
      
      WRITE(1,*)' '
      CALL PRTVEC(X,N,'VETOR X ATUAL')
      
      IF(MAX)THEN
      	WRITE(1,10)F
      ELSE
      	WRITE(1,10)-F
      ENDIF
      
      CALL PRTVEC(XP,N,'ESTIMATIVA DO VETOR X')
      WRITE(1,*)'PONTOS REJEITADOS - FORA DOS LIMITES'
      	
 
 10   FORMAT(/,'VALOR ATUAL DA FUNCAO',G25.18)    
 
      RETURN
      END 	 
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================

      SUBROUTINE PRT4(MAX,N,XP,X,FP,F)
      
      INTEGER N
      DOUBLE PRECISION XP(*),X(*),FP,F
      LOGICAL MAX
      
      WRITE(1,*)' '
      CALL PRTVEC(X,N,'VETOR X ATUAL')
      
      IF(MAX)THEN
      	WRITE(1,10)F
      	CALL PRTVEC(XP,N,'ESTIMATIVA DO VETOR X')
      	WRITE(1,20)FP
      ELSE
      	WRITE(1,10)-F
      	CALL PRTVEC(XP,N,'ESTIMATIVA DO VETOR X')
      	WRITE(1,20)-FP
      ENDIF     	
      	
 10   FORMAT(/,'VALOR ATUAL DE F : ',G25.18)
 20   FORMAT(/,'VALOR CORRIGIDO DE F : ',G25.18)     	

      RETURN
      END
      

C     ==================================================================
C     ******************************************************************
C     ==================================================================
      
      
      SUBROUTINE PRT5
      
      
      IF(IJI.EQ.0)THEN
        WRITE(1,*)' '
      	WRITE(1,*)'FORAM FEITAS MUITAS AVALIACOES DA FUNCAO.'
      	WRITE(1,*)'HA DUAS OPCOES :'
      	WRITE(1,*)'(1) AUMENTAR OS VALORES DE MAXEVL OU EPS ; '
      	WRITE(1,*)'(2) DIMINUIR OS VALORES DE NT OU RT. '
      	WRITE(1,*)' '
      ELSE
      	WRITE(1,*)' '
      	WRITE(1,*)'FORAM FEITAS MUITAS AVALIACOES DA FUNCAO.'
      	WRITE(1,*)'HA DUAS OPCOES :'
      	WRITE(1,*)'(1) AUMENTAR OS VALORES DE MAXEVL OU EPS ; '
      	WRITE(1,*)'(2) DIMINUIR OS VALORES DE NT OU RT. '
      	WRITE(1,*)' '
      ENDIF
      
      RETURN
      END
      	
            
C     ==================================================================
C     ******************************************************************
C     ==================================================================      

      SUBROUTINE PRT6(MAX)

      LOGICAL MAX   
      
         	
      IF(MAX)THEN
      	WRITE(1,*)'APESAR DE MENOR, O PONTO E ACEITO'
      ELSE
        WRITE(1,*)'APESAR DE MAIOR, O PONTO E ACEITO'
      ENDIF
      
      RETURN
      END
      
               
C     ==================================================================
C     ******************************************************************
C     ==================================================================      
      
      SUBROUTINE PRT7(MAX)
            
      LOGICAL MAX   
      
         	
      IF(MAX)THEN
      	WRITE(1,*)'PONTO INFERIOR REJEITADO'
      ELSE
        WRITE(1,*)'PONTO SUPERIOR REJEITADO'
      ENDIF
      
      RETURN
      END
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================      
      
      SUBROUTINE PRT8(N,VM,XOPT,X)

      INTEGER N
      DOUBLE PRECISION VM(*),XOPT(*),X(*)
      
      WRITE(1,*)' '
      WRITE(1,*)'RESULTADOS INTERMEDIARIOS DEPOIS DO AJUSTE DO TAMANHO'
      WRITE(1,*)'DO PASSO - VM (STEP LENGTH ADJUSTMENT)'
      WRITE(1,*)' '
      CALL PRTVEC(VM,N,'NOVO VETOR VM')
      CALL PRTVEC(XOPT,N,'VETOR X OTIMO ATUAL')
      CALL PRTVEC(X,N,'VALOR ATUAL DO VETOR X')
      WRITE(1,*)' '
      
      RETURN
      END
           
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================      


      SUBROUTINE PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)

      INTEGER N,NUP,NDOWN,NREJ,LNOBDS,NNEW,TOTMOV
      DOUBLE PRECISION XOPT(*),VM(*),T,FOPT
      LOGICAL  MAX
      
      TOTMOV = NUP + NDOWN + NREJ
      
      WRITE(1,*)' '
      WRITE(1,*)'******************************************************'
      WRITE(1,*)' '
      WRITE(1,*)'RESULTADOS INTERMEDIARIOS ANTES DA IMPLEMENTACAO DO '
      WRITE(1,*)'REDUTOR DE TEMPERATURA - RT'
      WRITE(1,10)T
 10   FORMAT(/,'TEMPERATURA ATUAL : ',G12.5)
      WRITE(1,*)' '
       
      IF(MAX)THEN
      	
      	WRITE(1,20)FOPT
 20     FORMAT('VALOR MAXIMO DA FUNCAO : ',G25.18) 
 
        WRITE(1,30)TOTMOV
 30     FORMAT('TOTAL DE MOVIMENTOS : ',I8)
 
        WRITE(1,40)NUP
 40     FORMAT('MOVIMENTOS PARA CIMA : ',I8)
 
        WRITE(1,50)NDOWN
 50     FORMAT('MOVIMENTOS ACEITOS PARA BAIXO : ',I8)                            	

        WRITE(1,60)NREJ
 60     FORMAT('MOVIMENTOS REJEITADOS PARA BAIXO : ',I8)  
 
        WRITE(1,70)LNOBDS
 70     FORMAT('TENTATIVAS FORA DOS LIMITES : ',I8)
 
        WRITE(1,80)NNEW
 80     FORMAT('NEW MAXIMA THIS TEMPERATURE : ',I8)
 
      ELSE                                          	

      	WRITE(1,90)-FOPT
 90     FORMAT('VALOR MINIMO DA FUNCAO : ',G25.18) 
 
        WRITE(1,100)TOTMOV
 100    FORMAT('TOTAL DE MOVIMENTOS : ',I8)
 
        WRITE(1,120)NUP
 120    FORMAT('MOVIMENTOS PARA BAIXO : ',I8)
 
        WRITE(1,130)NDOWN
 130    FORMAT('MOVIMENTOS ACEITOS PARA CIMA : ',I8)                            	

        WRITE(1,140)NREJ
 140    FORMAT('MOVIMENTOS REJEITADOS PARA CIMA : ',I8)  
 
        WRITE(1,150)LNOBDS
 150    FORMAT('TENTATIVAS FORA DOS LIMITES : ',I8)
 
        WRITE(1,160)NNEW
 160    FORMAT('NEW MINIMA THIS TEMPERATURE : ',I8)
 
      END IF
      
      
      WRITE(1,*)' '
      CALL PRTVEC(XOPT,N,'VETOR X OTIMO ATUAL')
      CALL PRTVEC(VM,N,'VETOR VM')
      WRITE(1,*)' '
      	
      RETURN
      END
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================      

      SUBROUTINE PRT10
      
      WRITE(1,*)'CRITERIO DE PARADA ALCANCADO PARA O ALGORITMO S.A.'
      
      RETURN
      END
      
C     ==================================================================
C     ******************************************************************
C     ==================================================================
      
      SUBROUTINE PRTVEC(VECTOR,NCOLS,NAME)
      

C     ESTA SUBROTINA IMPRIME O VETOR (VECTOR).  OBSERVE QUE SE HOUVER 
C     MAIS DO QUE 10 ELEMENTOS NO VETOR, 10 ELEMENTOS SERAO IMPRESSOS
C     EM CADA LINHA.
      

      INTEGER NCOLS
      DOUBLE PRECISION VECTOR(NCOLS)
      CHARACTER *(*) NAME
      
      WRITE(1,1001) NAME

      IF(NCOLS.GT.10)THEN
      	LINES = INT(NCOLS/10.)
      	DO 100 I = 1,LINES
        	LL = 10*(I - 1)
 100    CONTINUE
        WRITE(1,1000) (VECTOR(J),J = 1+LL, 10+LL)
        WRITE(1,1000) (VECTOR(J),J = 11+LL, NCOLS)
      ELSE
         WRITE(1,1000) (VECTOR(J),J = 1, NCOLS)
      END IF
      
 1000 FORMAT(10(D12.5,1X))
 1001 FORMAT(/,40X,A)      	
      RETURN
      END
      
