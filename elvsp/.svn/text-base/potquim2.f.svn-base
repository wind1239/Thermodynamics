C     ******************************************************************
C     ******************************************************************

      SUBROUTINE POTQUIM(N,NAT,COMP,ZPQ,A,B,BS,DPBM,AS,DPAM,IER6,G,
     &  ATIVL)
 
      
C     ******************************************************************
C     ******************************************************************


      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'common.f'
      INCLUDE 'comcalc.f'
      
      
      DIMENSION DPAM(ND),DPBM(ND),TERM(ND)
      DIMENSION PPAR(ND),G(ND),DLCF(ND)
      DIMENSION CFUG(ND),COMP(ND),ATIVL(ND)
      
      
      RZ2 = DSQRT(2.0D0)
      
      
      
C     *******************
C     *******************      
      
      IF(IRM.EQ.1)THEN

C
C     *******************
C     *******************


      ZERRO1 = ZPQ - B
      ZERRO2 = (ZPQ/B + 1.0D0 - RZ2)/(ZPQ/B + 1.0D0 + RZ2)
      IF((ZERRO1.LT.0.0D0).OR.(ZERRO2.LT.0.0D0).OR.
     +         (ZPQ.LT.0.0D0).OR.(ZPQ.GT.1.0D0))THEN
         IER6 = 0
         DO 666 I = 1,N
           G(I) = 0.0D0
 666     CONTINUE
      ELSE
         IER6 = 1
         DO 1000 I = 1,N
       	   DLCF(I) = -DLOG(ZERRO1) + 1.0D0/BS*DPBM(I)*(ZPQ - 1.0D0) 
     +             + 1.0D0/(2.0D0*RZ2)*(AS/(R*Te*BS))*(1.0D0/AS
     +             *DPAM(I) - 1.0D0/BS*DPBM(I))*DLOG(ZERRO2)
     	   CFUG(I) = DEXP(DLCF(I))
     	   PPAR(I) = DLOG(COMP(I)*Pe)
     	   G(I) = R*Te*(DLCF(I) + PPAR(I))
 1000      CONTINUE
      ENDIF
      
      
C     *******************
C     *******************      
      
      ELSE

C
C     *******************
C     *******************

      TERM(1)=0.0D0
      TERM(2)=0.0D0

      WRITE(*,*) 'PASSEI 3A'
      WRITE(*,*)'COMP(1) :',COMP(1)
      WRITE(*,*)'COMP(2) :',COMP(2)
      WRITE(*,*)'AI(1) :',AI(1)
      WRITE(*,*)'AI(2) :',AI(2)
      WRITE(*,*)'DELT(1,1) :',DELT(1,1)
      WRITE(*,*)'DELT(1,2) :',DELT(1,2)
      WRITE(*,*)'DELT(2,1) :',DELT(2,1)
      WRITE(*,*)'DELT(2,2) :',DELT(2,2)
      

      DO 2100 I=1,N
        WRITE(*,*)'OI !!!!'
C        DO 2000 J=1,N
          WRITE(*,*)'OLAH !!!!'
C          TERM(I)=COMP(J)*DSQRT(AI(J)*AI(I))*(1.0D0-DELT(J,I))+TERM(I)
           TERM(1)=1.0D0
           TERM(2)=1.0D0
          WRITE(*,*) 'PASSEI 3B'
C 2000   CONTINUE
        DLCF(I)=BI(I)/BS*(ZPQ-1.0D0)-DLOG(ZPQ-B)-A/(2.0D0*RZ2*B)*
     &  (2*TERM(I)/AS-BI(I)/BS)*DLOG((ZPQ+2.414*B)/(ZPQ-0.414*B))
    	CFUG(I) = DEXP(DLCF(I))
     	PPAR(I) = DLOG(COMP(I)*Pe)
     	G(I) = R*Te*(DLCF(I) + PPAR(I))
     	WRITE(*,*) 'PASSEI 3C'
 2100 CONTINUE


C     *******************
C     *******************      
      
      ENDIF

C
C     *******************
C     *******************
      IF(NAT.EQ.1)THEN
          CALL ATIV(CFUG,COMP,ATIVL)
      ELSE
        ATIVL(1) = 0.0D0
        ATIVL(2) = 0.0D0
      ENDIF

      ITERP = ITERP + 1

      RETURN
      END

