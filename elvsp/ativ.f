C     *******************************************************************
C     *******************************************************************
C     
C     				ARQUIVO ATIV.F
C
C     *******************************************************************
C     *******************************************************************

      SUBROUTINE ATIV(CFUG,COMP,ATIVL)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION ATIVL(2),FUG(2),CFUG(2),COMP(2)
      DOUBLE PRECISION FRV(2),Pv2(2)
      DOUBLE PRECISION AA(2),BB(2),CC(2),DD(2),WW(2)
      DOUBLE PRECISION TCC(2),PVV(2),TRR(2),TAUR(2),DLPVV(2)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'

C     ================================================================
C
C     			MODELO DE CALCULO DA ATIVIDADE
C
C     se IATV = 0 ; eh empregado o metodo analitico, senao (IATV = 01)
C     eh usado o modelo de Flory-Huggins
C
C     ================================================================

      IF(IATV.EQ.0)THEN



      	TAU = 1.0D0 - Tr(1)

      	SUMP = APV*TAU + BPV*(TAU**1.5) + CPV*(TAU**3) + DPV*(TAU**6)
      	RZPV = SUMP/Tr(1)

      	PvW = Pc(1)*1.0D-2*DEXP(RZPV)

C     *****************************************************************************
C     METODO DOS DOIS FLUIDOS DE REFERENCIA  (PROPANO E OCTANO)USANDO A EQ. WAGNER
C     *****************************************************************************

      NN=2
      AA(1)=-6.72219D0
      AA(2)=-7.91211D0
      BB(1)=1.33236D0     
      BB(2)=1.38007D0
      CC(1)=-2.13868D0
      CC(2)=-3.80435D0
      DD(1)=-1.38551D0
      DD(1)=-4.50132
      WW(1)=0.153D0
      WW(2)=0.398D0
      TCC(1)=369.85D0
      TCC(2)=568.81D0

      DO 100 I=1,NN
        TRR(I)=Te/TCC(I)
        TAUR(I)=1.0D0-TRR(I)
        DLPVV(I)=(AA(I)+BB(I)*(TAUR(I)**(15/10))+CC(I)*TAUR(I)**3+
     &            DD(I)*TAUR(I)**6)/TRR(I)
 100  CONTINUE
      DDLN=DLPVV(1)+(DLPVV(2)+DLPVV(1))*(W(1)-WW(1))/(WW(2)-WW(1))
      Pv(1)=Pc(1)*DEXP(DDLN)
      write(*,*)'Pv(1) :',Pv(1)
   




C     ****************************************************
C     CALCULO DA PRESSAO DE VAPOR (METODO DE GOMEZ-THODOS)
C     ****************************************************
	
      Tbr=Tb(1)/Tc(1)
      write(*,*)'Tbr :',Tbr
      APV1=(1.0D0-1.0D0/Tbr)/(Tbr**7-1.0D0)
      write(*,*)'APV1 :',APV1
      HPV=Tbr*DLOG(1.0D-5*Pc(1)/1.01325D0)/(1.0D0-Tbr)
      write(*,*)'HPV :',HPV
      write(*,*)'ISOLV :', ISOLV

      IF(ISOLV.EQ.0)THEN
C     COMPONENTES APOLARES (ORGANICOS OU INORGANICOS)
      write(*,*)'entrei'
        BTPV1=(HPV**(25/10))*DEXP(0.0384D0*(HPV**(25/10)))       
        write(*,*)'BTPV1 :',BTPV1
        BTPV2=(DEXP(2272.44D0/(HPV**3)))
        write(*,*)'BTPV1 :',BTPV1
        BTPV=-4.267D0-221.79D0/BTPV1++3.8126D0/BTPV2
        write(*,*)'BTPV :',BTPV
        MPV=0.78425D0*DEXP(0.089315D0*HPV)-8.5217/
     &      (DEXP(0.74826D0*HPV))
        write(*,*)'MPV :',MPV
        BPV1=(1.0D0-1.0D0/(Tbr**MPV))/(Tbr**7-1.0D0)
        write(*,*)'BPV1 :',BPV1
        GAMMAPV=APV1*HPV+BPV1*BETAPV
        write(*,*)'GAMMAPV :',GAMMAPV
      ELSEIF(ISOLV.EQ.1)THEN
C     COMPONENTES POLARES (EXCETO AGUA E ALCOOIS)
        MPV=0.466*(Tc(1)**0.166)
        GAMMAPV=0.08594*DEXP(7.462D-4*Tc(1))
        BPV1=(1.0D0-1.0D0/(Tbr**MPV))/(Tbr**7-1.0D0)
        BETAPV=GAMMAPV/BPV1-APV1*HPV/BPV1
      ELSE
C     AGUA E ALCOOIS
        MPV=0.0052*(MMOL(1)**0.29)*(Tc(1)**0.72)
        GAMMAPV=2.464D0/MMOL(1)*DEXP(9.8D-6*MMOL(1)*Tc(1))
        BETAPV=GAMMAPV/BPV1-APV1*HPV/BPV1
      ENDIF

      DLPVR=BETAPV*(1.0/(Tr(1)**MPV)-1.0D0)+GAMMAPV*
     &             ((Tr(1)**7)-1.0D0)
      write(*,*)'DLPVR :',DLPVR
      Pv2(1)=Pc(1)*DEXP(DLPVR)
        
    

      write(*,*)'Pv2(1):',Pv2(1)
      write(*,*)'Pv(1) :',Pv(1)

C     ********************************************
C     CALCULO DO COEF. FUGACIDADE DE SATURACAO
C     ********************************************

      CALL FUGP(CFLPu)

      write(*,*)'CFLPu : ',CFLPu
C     write(*,*)'CFUG(1) : ',CFUG(1)


C     ***********************
C     CALCULO DA ATIVIDADE
C     ***********************

      ATIVL(1)=COMP(1)*Pe*CFUG(1)/(Pv(1)*CFLPu)
      ativ2=COMP(1)*Pe*CFUG(1)/(PvW*CFLPu)
      ativ3=COMP(1)*Pe*CFUG(1)/(Pv2(1)*CFLPu)
      ATIVL(2) = 0.0D0

      write(*,*)'ATIVL(1) : ',ATIVL(1)
      write(*,*)'ativ2 : ',ativ2
      write(*,*)'ativ3 : ',ativ3

      ELSE

C     *****************************************
C     CALCULO DA ATIVIDADE - FLORY-HUGGINS :
C     *****************************************

      	DENM = COMP(1) + SEGMT*COMP(2)
      	FRV(1) = COMP(1)/DENM
      	FRV(2) = SEGMT*COMP(2)/DENM

      	ATIVL(1)=FRV(1)*DEXP((1.0D0-1.0D0/SEGMT)*FRV(2)+
     #         PFR*(FRV(2)**2))
      	ATIVL(2) = 0.0D0

      ENDIF
      ITRAT = ITRAT + 1      
      RETURN
      END


